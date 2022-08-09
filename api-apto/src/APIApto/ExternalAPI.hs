{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE RecordWildCards, DataKinds #-}

module APIApto.ExternalAPI where

import           APIApto.AppMonad               ( AppIOM(unAppIOM)
                                                , IntAPISettings(..)
                                                )
import           APIApto.Apto.Client            ( RequesterWithID )
import           APIApto.Card.Query            as QC
                                                ( GetLastFourErrors(..)
                                                , getCardLastFour
                                                )
import           APIApto.Model                  ( ErrorCodes(..)
                                                , ErrorEnvelope(ErrorEnvelope)
                                                , IdempotencyKey(IdempotencyKey)
                                                , TranactionAuthRequest(..)
                                                , TrxDirection(..)
                                                )
import           APIApto.Model.Webhook          ( AptoTransaction(..)
                                                , AptoWebhook(..)
                                                , CardUpdate(..)
                                                , CardUpdateEvent(..)
                                                , SettlementNotify
                                                )
import           Control.Concurrent.MVar        ( MVar )
import           Control.Exception              ( catch )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import qualified Data.Aeson                    as A
import           Data.Aeson                     ( (.=) )
import qualified Data.ByteString               as B
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Text.Lazy                as TL
import           Data.UUID                      ( fromText
                                                , toText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Money                          ( Dense
                                                , defaultDecimalConf
                                                , denseFromDecimal
                                                )
import           Network.HTTP.Types             ( Status(..)
                                                , status401
                                                , status403
                                                , status404
                                                , status500
                                                )
import           Network.Wai                    ( Application
                                                , Request(..)
                                                )
import           Network.Wai.Handler.Warp       ( Port )
import           Network.Wai.Middleware.AddHeaders
                                                ( addHeaders )
import           Network.Wai.Middleware.HttpAuth
                                                ( extractBasicAuth )
import           Servant.API                    ( NoContent )
import           Servant.Client                 ( ClientEnv
                                                , ClientM
                                                , runClientM
                                                )
import           Shared.Models.Apto.Cardholder  ( AptoCardholderResponse(..)
                                                , KYCEvent(..)
                                                )
import           Shared.Models.Apto.Transaction ( AptoCardHolder(..)
                                                , AptoTransactionId(..)
                                                , TransactionDetails(..)
                                                , TransactionSource(..)
                                                )
import           Shared.Models.Card            as Card
import           Shared.Models.Cardholder       ( CardholderId(AptoPaymentsCH) )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Transaction     as Trx
                                                ( DeclineReason(..)
                                                , MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionId(..)
                                                )
import           Shared.Models.User             ( UserID(..)
                                                , UserModel(..)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                               as PA
                                                ( AuthResult(..)
                                                , RiskRules(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( ChangeCardStateBody(..)
                                                , ChangeKYCStateBody(..)
                                                , Routes(..)
                                                , TraceContext(..)
                                                , accountsRoutes
                                                , asClientM
                                                , randomTrace
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( AuthorizePurchaseBody(..)
                                                , Routes
                                                  ( _AuthorizeCardPurchase
                                                  , _QueryPurchaseSourceId
                                                  , _UpdatePurchase
                                                  )
                                                , UpdatePurchaseBody(..)
                                                , payAuthClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , addHeader
                                                , defaultHandler
                                                , file
                                                , finish
                                                , function
                                                , get
                                                , header
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , matchAny
                                                , middleware
                                                , next
                                                , param
                                                , post
                                                , request
                                                , scottyApp
                                                , status
                                                , text
                                                )

data ExtApiSettings = ExtApiSettings
  { inProd         :: Bool
  , port           :: Port
  , authSettings   :: (B.ByteString, B.ByteString)
  , aptoClient     :: RequesterWithID
  , toDie          :: MVar ()
  , accountsEnv    :: ClientEnv
  , paymentAuthEnv :: ClientEnv
  }

unauthorized :: ActionM a
unauthorized = do
  status status401
  json $ ErrorEnvelope [ErrorUnAuthorized]
  finish

unknownUser :: TL.Text -> ActionM a
unknownUser _ = do
  status status404
  json $ ErrorEnvelope [ErrorUnknownUser]
  finish

handleTransaction
  :: ClientEnv -> ClientEnv -> AptoTransaction -> IO (Either Status ())
handleTransaction accountsEnv _ notif@CardNotification {..} = do
  trace <- randomTrace
  let fn = _UsersQueryCardholder asClientM trace $ AptoPaymentsCH $ apcId
        notCardholder
  userEi <- runClientM fn accountsEnv
  putStr "Warning (handleTransaction) got CardNotification "
    >> print (userEi, notif)
  return $ Right ()
handleTransaction accountsEnv payAuthEnv AptoTransaction { aptSourceId = AptoTransactionId sourceId, aptCardholder = AptoCardHolder { apcId = cardholder }, ..}
  = do
    trace <- randomTrace

    let getUser =
          _UsersQueryCardholder asClientM trace $ AptoPaymentsCH cardholder
    let getPurchase = _QueryPurchaseSourceId payAuthClientM trace sourceId

    userEi <- runClientM getUser accountsEnv
    trxEi  <- runClientM getPurchase payAuthEnv

    case (userEi, trxEi) of
      (Left e, _) -> do
        putStr "Error (handleTransaction) Error getting Cardholder: "
          >> print (cardholder, e)
        return $ Left status500
      (Right [], _) -> do
        putStr "Error (handleTransaction) Missing Cardholder: "
          >> print cardholder
        return $ Left status500
      (Right (UserModel {..} : _), Left _) -> do
        trxId <- TransactionId <$> nextRandom
        putStr "Info (handleTransaction) Missing Trx, creating a new one: "
          >> print (usrUserID, trxId, cardholder, sourceId)
        res <- runClientM (sendTrx trace usrUserID trxId) payAuthEnv
        case res of
          Right _ -> return $ Right ()
          Left  e -> do
            putStr "Error: handleTransaction _UpdatePurchase "
              >> print (usrUserID, e)
            return $ Left status500
      (Right (UserModel {..} : _), Right Nothing) -> do
        trxId <- TransactionId <$> nextRandom
        putStr "Info (handleTransaction) Missing Trx, creating a new one: "
          >> print (usrUserID, trxId, cardholder, sourceId)
        res <- runClientM (sendTrx trace usrUserID trxId) payAuthEnv
        case res of
          Right _ -> return $ Right ()
          Left  e -> do
            putStr "Error: handleTransaction _UpdatePurchase "
              >> print (usrUserID, e)
            return $ Left status500
      (Right (UserModel {..} : _), Right (Just Transaction {..})) -> do
        res <- runClientM (sendTrx trace usrUserID trxId) payAuthEnv
        case res of
          Right _ -> return $ Right ()
          Left  e -> do
            putStr "Error: handleTransaction _UpdatePurchase "
              >> print (usrUserID, e)
            return $ Left status500
 where
  sendTrx :: TraceContext -> UserID -> TransactionId -> ClientM NoContent
  sendTrx trace userID trxId =
    _UpdatePurchase payAuthClientM trace trxId $ UpdatePurchaseBody
      { purchaser        = userID
      , transactionId    = trxId
      , source           = aptSource
      , idempotency      = aptIdempotency
      , sourceId         = sourceId
      , cardId           = AptoPayments aptCardId
      , details          = aptDetails
      , merchant         = aptMerchant
      , standin          = aptStandin
      , state            = aptState
      , createdAt        = aptCreatedAt
      , transactionEvent = aptTransactionEvent
      , amountLocal      = aptAmountLocal
      , amountHold       = aptAmountHold
      , amountCashback   = aptAmountCashback
      , amountFee        = aptAmountFee
      , amountBilling    = aptAmountBilling
      , description      = pcpDescription aptDetails
      , adjustments      = aptAdjustments
      }

handleKYC :: ClientEnv -> AptoCardholderResponse -> IO (Either Status ())
handleKYC accountsEnv msg@AptoCardholderResponse {..} = do
  trace <- randomTrace
  let fn = _UsersQueryCardholder asClientM trace (AptoPaymentsCH accxId)
  userCrd <- runClientM fn accountsEnv
  userCus <- case UserID <$> fromText accxPayTgthrId of
    Nothing -> return $ Left ("Not a UUID" :: String)
    Just u  -> do
      let getAUser = _UserGet $ accountsRoutes accountsEnv
      Right <$> getAUser trace u

  let uidMaybe = case (userCrd, userCus) of
        (Right (usr : _), _        ) -> Just $ usrUserID usr
        (_              , Right usr) -> Just $ usrUserID usr
        (Right []       , Left _   ) -> Nothing
        (Left  _        , Left _   ) -> Nothing

  case (uidMaybe, accxEvent) of
    (Nothing, _) -> do
      putStr "Error: User not found " >> print (userCrd, userCus, msg)
      return $ Left status500
    (_, Nothing) -> do
      putStr "Error: Must have event in accxEvent "
        >> print (accxId, accxPayTgthrId, msg)
      return $ Left status500
    -- Next
    (Just uid, Just KYCStatusUpdate) -> case accxKYCStatus of
      Nothing        -> return $ Right ()
      Just kycStatus -> do
        let setKycState = _UserSetKYCState asClientM trace uid
              $ ChangeKYCStateBody kycStatus
        res <- runClientM setKycState accountsEnv
        case res of
          Left e -> do
            putStr "Error: handleKYC _UserSetKYCState "
              >> print (uid, e, accxId, accxPayTgthrId, msg)
            return $ Left status500
          Right _ -> return $ Right ()
     -- Next
    (_, Just KYCUserUpdate) -> do
      putStr "Error: (KYC Unhandled) " >> print msg
      return $ Left status500
     -- Next
    (Just uid, Just KYCIdentityUpdate) -> case accxKYCStatus of
      Nothing        -> return $ Right ()
      Just kycStatus -> do
        let setKycState = _UserSetKYCState asClientM trace uid
              $ ChangeKYCStateBody kycStatus
        res <- runClientM setKycState accountsEnv
        case res of
          Left e -> do
            putStr "Error: handleKYC _UserSetKYCState "
              >> print (uid, e, accxId, accxPayTgthrId, msg)
            return $ Left status500
          Right _ -> return $ Right ()

handleCard :: ClientEnv -> CardUpdate -> IO (Either Status ())
handleCard accountsEnv msg@CardUpdate {..} = do
  trace <- randomTrace
  let fn =
        _UsersQueryCardholder asClientM trace $ AptoPaymentsCH crdCardholderId
  userEither <- runClientM fn accountsEnv

  case (userEither, crdEvent) of
    (Left e, _) -> do
      putStr "Error: _UsersQueryCardholder err "
        >> print (crdCardholderId, msg, e)
      return $ Left status500
    (Right [], _) -> do
      putStr "Error: Cardholder not found " >> print (crdCardholderId, msg)
      return $ Left status500
    (Right (UserModel{}    : _), PinUpdated  ) -> return $ Right ()
    (Right (UserModel {..} : _), StatusUpdate) -> do
      let issuerId = AptoPayments crdId
      let design   = Just crdDesign

      let findCard = _FindCard asClientM trace issuerId
      foundCardRes <- runClientM findCard accountsEnv
      case foundCardRes of
        Left e -> do
          putStr "Error: handleCard _FindCard " >> print (usrUserID, e, trace)
          return $ Left status500
        Right Nothing -> do
          putStr "Error: handleCard _FindCard found Nothing "
            >> print (usrUserID, trace)
          return $ Left status500
        Right (Just CardModel {..}) -> do
          let setCardState =
                _UserSetCardState asClientM trace usrUserID cardId
                  $ ChangeCardStateBody crdStatus design Nothing
          res <- runClientM setCardState accountsEnv
          case res of
            Right _ -> return $ Right ()
            Left  e -> do
              putStr "Error: handleCard _UserSetCardState "
                >> print (usrUserID, e, trace)
              return $ Left status500
    (Right (UserModel{} : _), CardShipped) -> return $ Right ()

handleSettlement :: SettlementNotify -> IO (Either Status ())
handleSettlement m =
  putStr "Error: Noop on handleSettlement" >> print m >> return (Left status500)

handleWebhook :: ExtApiSettings -> ActionM ()
handleWebhook ExtApiSettings {..} = do
  incoming :: AptoWebhook <- jsonData

  liftAndCatchIO $ putStr "Webhook: " >> print incoming

  res <- liftAndCatchIO $ case incoming of
    TransactionNot trx  -> handleTransaction accountsEnv paymentAuthEnv trx
    KYCNot         kyc  -> handleKYC accountsEnv kyc
    CardNot        card -> handleCard accountsEnv card
    SettlementNot  set  -> handleSettlement set

  case res of
    Left s -> do
      liftAndCatchIO $ putStr "Error: (handleWebhook) " >> print (s, incoming)
      status s >> text "Error"
    Right _ -> text "Ok"

data AuthError
  = MissingGroup
  | BalanceFail
  deriving (Eq, Show)

failCheck :: Either AuthError b -> ActionM b
failCheck (Left MissingGroup) = unknownUser ""
failCheck (Left BalanceFail) =
  status status403 >> json (ErrorEnvelope [ErrorBalanceCheckFailed]) >> finish
failCheck (Right x) = return x

data AuthorizationDecision
  = ApproveAuth TransactionId
  | DeclineAuth TransactionId DeclineReason
  deriving (Eq, Show)

handleAuthDebit
  :: ClientEnv
  -> UserID
  -> TranactionAuthRequest
  -> TransactionId
  -> IO AuthorizationDecision
handleAuthDebit payAuthEnv uid req newTransactionId = do
  trace <- randomTrace
  let amountRead :: Dense "USD" =
        fromJust . denseFromDecimal defaultDecimalConf $ tarAmount req
  let amount = toRational amountRead
  let authMsg = AuthorizePurchaseBody
        { transactionId     = newTransactionId
        , source            = Apto
        , sourceId          = extractAptoTrxID $ tarTransactionId req
        , sourceIdempotency = Just . extractIdemKey $ tarIdempotencyKey req
        , amount            = Currency (tarCurrency req) amount
        , description       = tarDescription req
        , merchant          = tarMerchant req
        , details           = tarDetails req
        , cardUsed = AptoPayments $ fromMaybe (AptoCardId "") (tarCardId req)
        }

  let authorize = _AuthorizeCardPurchase payAuthClientM trace uid authMsg
  response <- runClientM authorize payAuthEnv
  resBody  <- case response of
    Left  e    -> error $ "Could not authroize " <> show (req, e)
    Right body -> return body

  let declined = DeclineAuth newTransactionId

  return $ case resBody of
    Success t        -> ApproveAuth (trxId t)
    UserMissing _ _  -> declined $ UserNotFound []
    GroupNotActive _ -> declined GroupError
    GroupNotFound  _ -> declined GroupError
    BalanceRequestFailed _ _ -> declined $ BalanceCheckFail []
    InsufficentFunds _ _ -> declined $ LowBalance []
    NoPaymentLinked _ _ -> declined $ PaymentUnlinked []
    AccountClosed _ _ -> declined Trx.CardClosed
    AccountNotActive _ _ -> declined $ UserNotActive []
    RiskTriggered _ RiskyMerchantName -> declined RiskyMerchant
    RiskTriggered _ RiskyMerchantMCC -> declined RiskyMerchant
    RiskTriggered _ RiskyMerchantCountry -> declined RiskyMerchant
    RiskTriggered _ RiskyP2P -> declined P2PNotAllowed
    RiskTriggered _ CombinedRiskRules -> declined RiskyMerchant
    RiskTriggered _ (ExceedMaxPerTrxAmount c) ->
      declined $ ExceedMaxTrxAmount c
    PA.CardNotActivated _ _ -> declined Trx.CardNotActivated
    CardFrozen          _ _ -> declined CardInactive
 where
  extractAptoTrxID (AptoTransactionId t) = t
  extractIdemKey (IdempotencyKey t) = t

-- brittany-next-binding --columns 500
handleAuthCredit :: UserID -> TranactionAuthRequest -> TransactionId -> IO AuthorizationDecision
handleAuthCredit uid req tid = do
  putStr "Credit transactions: (Uid,Tid,Req,Mcc) " >> print (uid, tid, req, cmiMcc <$> tarMerchant req)

  let declined = return . DeclineAuth tid
  case tarMerchant req of
    (Just CardMerchant { cmiMcc = MastercardMCC "4829" }) -> declined P2PNotAllowed -- Money Transfer
    (Just CardMerchant { cmiMcc = MastercardMCC "6010" }) -> declined P2PNotAllowed -- Manual Cash Disbursements
    (Just CardMerchant { cmiMcc = MastercardMCC "6011" }) -> declined P2PNotAllowed -- Automated Cash disbursements
    (Just CardMerchant { cmiMcc = MastercardMCC "6050" }) -> declined P2PNotAllowed -- Quasi Cash
    (Just CardMerchant { cmiMcc = MastercardMCC "6051" }) -> declined P2PNotAllowed -- Quasi Cash
    (Just CardMerchant { cmiMcc = MastercardMCC "6211" }) -> declined P2PNotAllowed -- Securities
    (Just CardMerchant { cmiMcc = MastercardMCC "6532" }) -> declined P2PNotAllowed -- Payment Transaction
    (Just CardMerchant { cmiMcc = MastercardMCC "6533" }) -> declined P2PNotAllowed -- Payment Transaction
    (Just CardMerchant { cmiMcc = MastercardMCC "6536" }) -> declined P2PNotAllowed -- MoneySend (Incoming)
    (Just CardMerchant { cmiMcc = MastercardMCC "6537" }) -> declined P2PNotAllowed -- MoneySend
    (Just CardMerchant { cmiMcc = MastercardMCC "6538" }) -> declined P2PNotAllowed -- MoneySend
    (Just CardMerchant { cmiMcc = MastercardMCC "6540" }) -> declined P2PNotAllowed -- POI Funding
    (Just CardMerchant { cmiMcc = MastercardMCC _ }) -> return $ ApproveAuth tid
    Nothing -> return $ ApproveAuth tid

handleAuth
  :: ExtApiSettings
  -> TranactionAuthRequest
  -> UserID
  -> IO AuthorizationDecision
handleAuth ExtApiSettings {..} req uid = do
  newTransactionId <- TransactionId <$> nextRandom

  case tarType req of
    DebitTransaction  -> handleAuthDebit paymentAuthEnv uid req newTransactionId
    CreditTransaction -> handleAuthCredit uid req newTransactionId

debugLastFour :: ExtApiSettings -> ActionM ()
debugLastFour ExtApiSettings {..} = do
  user  <- UserID . fromJust . fromText <$> param "userid"
  trace <- randomTrace

  let mockedDecrypter = const $ error "not used"

  res :: Either GetLastFourErrors CardLastFour <- liftAndCatchIO
    (       (Right <$> runReaderT
              (unAppIOM $ getCardLastFour trace user)
              (IntAPISettings aptoClient mockedDecrypter mockedDecrypter accountsEnv)
            )
    `catch` (return . Left)
    )

  case res of
    Left e -> do
      liftAndCatchIO $ putStr "Error: debugLastFour " >> print (user, e)
      status status500 >> finish
    Right (CardLastFour t) -> json $ A.object ["lastfour" .= t]

defaultErrorHandler :: ExtApiSettings -> TL.Text -> ActionM ()
defaultErrorHandler ExtApiSettings{} err = do
  req  <- request
  uuid <- liftAndCatchIO nextRandom
  liftAndCatchIO $ putStr "Error at defaultHandler: " >> print
    (err, requestMethod req, rawPathInfo req, requestHeaders req, uuid)
  status status500
  Scotty.addHeader "X-Error" err
  text $ "500 error; trace: " <> TL.fromStrict (toText uuid)
  -- liftAndCatchIO $ putMVar toDie ()

externalApiApp :: ExtApiSettings -> IO Application
externalApiApp config@ExtApiSettings {..} = scottyApp $ do
  defaultHandler $ defaultErrorHandler config

  middleware $ addHeaders [("Cache-Control", "no-cache")]

  get "/ping" $ text "pong"

  -- All paths under here are basic auth protected
  matchAny (function . const . Just $ []) $ do
    auth <- header "Authorization"
    case auth <&> encodeUtf8 . TL.toStrict >>= extractBasicAuth of
      Just (u, p) -> if u == fst authSettings && p == snd authSettings
        then next
        else unauthorized
      Nothing -> unauthorized

  get "/swagger.yaml" $ file "public/swagger.yaml"
  get "/v1/_health" $ text "Ok"

  post "/v1/webhook" $ handleWebhook config

  -- V1 and V2 of Auth
  post "/v1/:userid/transactions" $ status status403 >> json
    (ErrorEnvelope [ErrorDeclined "No longer accepted"])

  post "/v3/authorize" $ status status403 >> json
    (ErrorEnvelope [ErrorDeclined "No longer accepted"])

  -- Internal
  get "/debug/v1/lastfour" $ debugLastFour config
  get "/debug/v1/lastfour/:userid" $ debugLastFour config

