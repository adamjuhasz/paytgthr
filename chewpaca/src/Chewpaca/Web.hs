{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards, StrictData #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Chewpaca.Web where

import           Chewpaca.DB.Cards              ( getCardId
                                                , getCardsFor
                                                )
import           Chewpaca.DB.Groups             ( getGroupById
                                                , getGroups
                                                , getGroupsById
                                                , getGroupsForUser
                                                )
import           Chewpaca.DB.KYC                ( getUsersKYCAssesments )
import           Chewpaca.DB.Ledger             ( getAllEntriesForUser
                                                , getAllJournalsOf
                                                , getEntireLedger
                                                , getEntriesForJournal
                                                , getJournal
                                                , getLedgerForUser
                                                , getUserJournals
                                                )
import           Chewpaca.DB.PaymentAuth        ( getAllTransactions
                                                , getBalancesForUser
                                                , getMatchTestTransactions
                                                , getSingleTransactionRevs
                                                , getTransactions
                                                , getUsersTransactions
                                                )
import           Chewpaca.DB.Rewards            ( getRewardBoostActivations )
import           Chewpaca.DB.RiskScores         ( getCurrentRiskForUser
                                                , getRisksForUser
                                                )
import           Chewpaca.DB.Tokens             ( getTokensForUser )
import           Chewpaca.DB.Users              ( getJustUser
                                                , getUser
                                                , getUserNote
                                                , getUsers
                                                , getUsersWithIds
                                                , saveUser
                                                , searchUser
                                                , searchUsersAddress
                                                , searchUsersBanking
                                                )
import           Chewpaca.Reports.Generate      ( generateEarnedRewards
                                                , generateWeekly
                                                , generateWeeklyUser
                                                )
import           Chewpaca.Tailwind.Frame        ( CurrentSection(..)
                                                , TransactionPageTypes(..)
                                                , UserPageTypes(..)
                                                , appShell
                                                )
import           Chewpaca.Users.AppEvents       ( getAppEventsForDevice
                                                , getAppEventsForUser
                                                , getDevicesWithAppEvents
                                                , getUsersWithAppEvents
                                                )
import           Chewpaca.Users.TodayList       ( signedUpLast24Hours )
import           Chewpaca.Web.Dashboard         ( renderDashboard )
import           Chewpaca.Web.Group             ( renderGroup )
import           Chewpaca.Web.Ledger.Journal    ( renderJournalList )
import           Chewpaca.Web.Payment           ( renderPayment )
import           Chewpaca.Web.Payments          ( renderPaymentList )
import           Chewpaca.Web.Rewards.Rewards   ( renderRewardMatch )
import           Chewpaca.Web.Transaction       ( renderTransaction )
import           Chewpaca.Web.Transactions.Stats
                                                ( renderTrxStats )
import           Chewpaca.Web.User              ( UserInfo(..)
                                                , renderUser
                                                )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async
                                                , wait
                                                )
import           Control.Exception              ( SomeException(..)
                                                , catch
                                                )
import           Control.Monad                  ( when )
import           Data.Aeson                    as Aeson
                                                ( (.=)
                                                , Value(..)
                                                , decode
                                                , object
                                                )
import qualified Data.ByteString.Char8         as S8
import qualified Data.ByteString.Lazy          as BL
import           Data.Coerce                    ( coerce )
import           Data.Csv                      as CSV
                                                ( DefaultOrdered(..)
                                                , ToNamedRecord(..)
                                                , encodeDefaultOrderedByName
                                                , header
                                                , namedField
                                                , namedRecord
                                                )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( nub )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , listToMaybe
                                                )
import           Data.Pool                      ( withResource )
import           Data.Ratio                     ( (%) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Text.Encoding            as TE
                                                ( decodeUtf8 )
import qualified Data.Text.Lazy                as TL
import           Data.Time                      ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format.ISO8601       ( iso8601ParseM )
import           Data.UUID                     as U
                                                ( UUID
                                                , fromText
                                                , nil
                                                , toText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Vault.Lazy               as V
import           Network.Connection             ( TLSSettings(..) )
import           Network.HTTP.Client            ( Manager
                                                , newManager
                                                )
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Network.HTTP.Types             ( hContentType
                                                , status200
                                                , status400
                                                , status404
                                                , status500
                                                )
import           Network.Wai                   as Wai
                                                ( Application
                                                , Request(..)
                                                , Response
                                                , responseLBS
                                                )
import           Network.Wai.Handler.Warp       ( Port
                                                , defaultSettings
                                                , getPort
                                                , runSettings
                                                , setOnExceptionResponse
                                                , setPort
                                                )
import           Network.Wai.Middleware.AddHeaders
                                                ( addHeaders )
import           Network.Wai.Middleware.Cors    ( CorsResourcePolicy(..)
                                                , cors
                                                )
import           Network.Wai.Middleware.Gzip    ( GzipFiles(..)
                                                , GzipSettings(..)
                                                , def
                                                , gzip
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( OutputFormat(..)
                                                , RequestLoggerSettings(..)
                                                , mkRequestLogger
                                                )
import           Network.Wai.Middleware.Static  ( (>->)
                                                , CachingStrategy(NoCaching)
                                                , Options(..)
                                                , addBase
                                                , defaultOptions
                                                , initCaching
                                                , noDots
                                                , staticPolicyWithOptions
                                                )
import           Network.Wai.Parse              ( parseHttpAccept )
import           Servant.API                    ( NoContent(NoContent) )
import           Servant.Client                 ( mkClientEnv
                                                , parseBaseUrl
                                                , runClientM
                                                )
import           Shared.Amqp                    ( CommandMessage(..)
                                                , EventMessage(..)
                                                , MessageBody(..)
                                                , OpaqueChannel
                                                , ReplyMessage(..)
                                                , ReplySuccess(..)
                                                , TgthrMessage(..)
                                                , publish
                                                , publishWithReply
                                                )
import           Shared.Amqp.Utils              ( getGroupForUser )
import           Shared.Console                 ( traceError )
import qualified Shared.DB.Ledger              as LDB
import           Shared.DB.PartnerInvite
import           Shared.DB.Payment              ( getAllPayments
                                                , getPaymentRevisions
                                                , getPaymentsForUser
                                                , getUsersPaymentRevs
                                                , getUsersPendingPayments
                                                , loadPayment
                                                )
import           Shared.DB.Referral             ( getAllReferralCodes
                                                , getAllReferralPrograms
                                                , getAllReferralProgress
                                                , getReferralCode
                                                , getReferralCodeForUser
                                                , getReferralProgram
                                                , getReferralProgressOf
                                                , getReferralProgressRevisions
                                                , getReferreeProgressFor
                                                )
import           Shared.Database                ( PooledDB )
import           Shared.Models.Card             ( CardModel(..)
                                                , CardStatus(..)
                                                , IssuerPlatform(..)
                                                )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                , roundDownUSD
                                                )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupMember(..)
                                                , GroupModel(..)
                                                , GroupSplit(..)
                                                )
import           Shared.Models.Ids              ( ActivatedRewardId(..)
                                                , CardId(..)
                                                , JournalId(JournalId)
                                                , LedgerEntryId(LedgerEntryId)
                                                , LedgerTrxId(LedgerTrxId)
                                                , PaymentId(PaymentId)
                                                , ReferralProgramID(..)
                                                , ReferralProgressID
                                                  ( ReferralProgressID
                                                  )
                                                , RewardId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.KYC              ( KYCFailureReasons(..)
                                                , KycStatus(..)
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..)
                                                , factAmount
                                                , factLink
                                                , factType
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , JournalType(..)
                                                , journalTag
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentMethod(..)
                                                , PaymentStatus(..)
                                                , PaymentSubType(NormalPayment)
                                                )
import           Shared.Models.Referral.ReferralProgram
                                                ( ReferralProgram(..) )
import           Shared.Models.Referral.ReferralProgress
                                                ( ReferralProgress
                                                , WorkFlowProgress
                                                )
import           Shared.Models.Rewards.Boost    ( RewardBoost(..) )
import           Shared.Models.Rewards.Categorizer
                                                ( categorize )
import           Shared.Models.RiskScore        ( RiskFact(ManualRiskAdj)
                                                , RiskScore(rskTrustScore)
                                                , dollarToNextLevel
                                                , exampleRiskScore
                                                , getUserLevelFromScore
                                                , limitByLevel
                                                , progressToNextLevel
                                                )
import           Shared.Models.Token            ( ExpirationTime(TenMinutes)
                                                , TokenId(TokenId)
                                                , TokenMedium(..)
                                                )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionId(..)
                                                )
import           Shared.Models.User             ( ClosureReason(..)
                                                , EmailAddress(..)
                                                , PhoneNumber(..)
                                                , RedactedText(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                , UserTrait(..)
                                                , normalizeEmail
                                                )
import           Shared.TgthrMessages.Base      ( fromMessageID )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PaymentCmd(..)
                                                , PaymentEvents(..)
                                                , PaymentReplies(..)
                                                )
import           Shared.Track.Stackdriver       ( stackDriverMiddleware )
import           Shared.Vault                  as V
                                                ( CipherText(..)
                                                , PlainText(..)
                                                )
import           Shared.Web.RequestLogger       ( formatAsJSONWithHeaders )
import           Shared.WebAPI.AccountsFSM.API  ( ActivateRewardBody(..)
                                                , incrementTrace
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( ChangeKYCStateBody(..)
                                                , CloseGroupBody(..)
                                                , CloseUserBody(..)
                                                , CreateGroupBody(..)
                                                , RemoveBankFSBody(..)
                                                , Routes(..)
                                                , TraceContext
                                                , UpdateUserBody(..)
                                                , accountsClientM
                                                , accountsRoutes
                                                , randomTrace
                                                )
import           Shared.WebAPI.ApiPrivacy.Client
                                                ( Routes(_CardHostedUI)
                                                , pricacyClientM
                                                )
import           Shared.WebAPI.General.API      ( parseTrace )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( CreateLedgerJournalBody(..)
                                                , CreateLedgerTrxBody(..)
                                                , MakeVerificationPaymentBody(..)
                                                , Routes(..)
                                                , SetManualFSBody(..)
                                                , UpdatePaymentBody(..)
                                                , VerificationPaymentStyle(..)
                                                , payAuthClientM
                                                )
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )
import           Text.Mustache                  ( Template(templateActual)
                                                , compileMustacheDir
                                                , renderMustacheW
                                                )
import           Text.Pretty.Simple             ( pShowNoColor )
import           Web.JWT                        ( Signer )
import qualified Web.Scotty                    as Scotty
import           Web.Scotty                     ( ActionM
                                                , defaultHandler
                                                , delete
                                                , file
                                                , finish
                                                , get
                                                , html
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , middleware
                                                , next
                                                , notFound
                                                , param
                                                , post
                                                , put
                                                , raw
                                                , redirect
                                                , regex
                                                , request
                                                , rescue
                                                , scottyApp
                                                , setHeader
                                                , status
                                                , text
                                                )

instance ToNamedRecord Transaction where
  toNamedRecord Transaction { trxId = TransactionId tid, trxUserId = UserID uid, trxDisplayAmount = Currency _ displayAmt, ..}
    = namedRecord
      [ "id" `namedField` U.toText tid
      , "state" `namedField` show trxState
      , "source_id" `namedField` (coerce trxSourceId :: Text)
      , "user_id" `namedField` U.toText uid
      , "group_id" `namedField` (U.toText . coerce . fst <$> trxGroupId)
      , "display_amount" `namedField` (fromRational displayAmt :: Double)
      , "purchased_at" `namedField` show trxPurchasedAt
      , "description" `namedField` trxDescription
      , "merchant_mcc"
        `namedField` ((coerce . cmiMcc <$> trxMerchant) :: Maybe Text)
      , "merchant_name" `namedField` (cmiName <$> trxMerchant)
      , "merchant_locality" `namedField` (trxMerchant >>= cmiLocality)
      , "merchant_region" `namedField` (trxMerchant >>= cmiRegion)
      , "context" `namedField` (pcpContext <$> trxDetails)
      , "card_a" `namedField` (show . pcpIsCardPresent <$> trxDetails)
      , "ecommerce" `namedField` (show . pcpIsOnline <$> trxDetails)
      , "emv" `namedField` (show . pcpIsEMV <$> trxDetails)
      , "transaction_description" `namedField` (trxDetails >>= pcpDescription)
      , "merchant_mcc_category" `namedField` (cmiMccDesc <$> trxMerchant)
      ]

instance DefaultOrdered Transaction where
  headerOrder _ = CSV.header
    [ "id"
    , "state"
    , "source_id"
    , "user_id"
    , "group_id"
    , "display_amount"
    , "purchased_at"
    , "description"
    , "merchant_mcc"
    , "merchant_name"
    , "merchant_locality"
    , "merchant_region"
    , "context"
    , "card_a"
    , "ecommerce"
    , "emv"
    , "transaction_description"
    , "merchant_mcc_category"
    ]

instance ToNamedRecord UserModel where
  toNamedRecord UserModel { usrUserID = UserID uid, ..} = namedRecord
    [ "id" `namedField` U.toText uid
    , "state" `namedField` show usrUserState
    , "email" `namedField` (coerce usrEmail :: Text)
    , "name_first" `namedField` usrFirstName
    , "name_last" `namedField` usrLastName
    , "address_street" `namedField` usrAddressStreet
    , "address_street_2" `namedField` usrAddressStreet2
    , "address_city" `namedField` usrAddressCity
    , "address_state" `namedField` usrAddressState
    , "address_zip" `namedField` usrAddressZip
    , "phone" `namedField` (coerce <$> usrPhone :: Maybe Text)
    , "dob" `namedField` (show <$> usrDOB)
    , "ssn" `namedField` (coerce <$> usrSSN :: Maybe Text)
    , "bank_aba" `namedField` (coerce <$> usrBankRouting :: Maybe Text)
    , "bank_dda" `namedField` (coerce <$> usrBankAcount :: Maybe Text)
    , "bank_name" `namedField` usrBankName
    , "bank_account_name" `namedField` usrBankAccountName
    , "bank_verified" `namedField` (show <$> usrBankVerified)
    , "apto_kyc_status" `namedField` (show <$> usrAptoKYCStatus)
    , "apto_card_a" `namedField` (show <$> usrAptoCardStatus)
    , "apto_cardholder_id" `namedField` (show <$> usrAptoCardholderID)
    , "date_user_created" `namedField` show usrCreatedOn
    , "date_user_first_signin" `namedField` (show <$> usrFirstSignIn)
    , "date_user_card_a" `namedField` (show <$> usrCardCreatedOn)
    , "date_user_card_a" `namedField` (show <$> usrCardActivatedOn)
    ]

instance DefaultOrdered UserModel where
  headerOrder _ = header
    [ "id"
    , "state"
    , "email"
    , "name_first"
    , "name_last"
    , "address_street"
    , "address_street_2"
    , "address_city"
    , "address_state"
    , "address_zip"
    , "phone"
    , "dob"
    , "ssn"
    , "bank_aba"
    , "bank_dda"
    , "bank_name"
    , "bank_account_name"
    , "bank_verified"
    , "apto_kyc_status"
    , "apto_card_a"
    , "apto_cardholder_id"
    , "date_user_created"
    , "date_user_first_signin"
    , "date_user_card_a"
    , "date_user_card_a"
    ]

instance ToNamedRecord LedgerEntry where
  toNamedRecord LedgerEntry { lenId = LedgerEntryId lid, lenUser = user, lenBalance = Currency _ value, ..}
    = namedRecord
      [ "lenId" `namedField` U.toText lid
      , "lenUser"
        `namedField` (case user of
                       Just (UserID u) -> U.toText u
                       Nothing         -> ""
                     )
      , "lenRevision" `namedField` lenRevision
      , "lenBalance" `namedField` (fromRational value :: Double)
      , "lenFact" `namedField` show lenFact
      , "lenFact_type" `namedField` factType lenFact
      , "lenFact_amount"
        `namedField` (fromRational (getMonetaryValue $ factAmount lenFact) :: Double
                     )
      , "lenFact_link" `namedField` factLink lenFact
      , "lenIdempotency" `namedField` show lenIdempotency
      , "lenCreatedAt" `namedField` show lenCreatedAt
      ]

instance DefaultOrdered LedgerEntry where
  headerOrder _ = header
    [ "lenId"
    , "lenUser"
    , "lenRevision"
    , "lenBalance"
    , "lenFact"
    , "lenFact_type"
    , "lenFact_amount"
    , "lenFact_link"
    , "lenIdempotency"
    , "lenCreatedAt"
    ]

data AppSettings = AppSettings
  { inProd          :: Bool
  , dbPool          :: PooledDB
  , port            :: Port
  , mqChannel       :: Maybe OpaqueChannel
  , publicDir       :: String
  , ssnDecrypt      :: CipherText -> IO V.PlainText
  , ssnEncrypt      :: V.PlainText -> IO CipherText
  , stackDriverInfo :: (Manager, (Text, Signer, Text))
  }

createTrace :: ActionM TraceContext
createTrace = do
  traceMaybe <- Scotty.header "X-Cloud-Trace-Context"
  case traceMaybe of
    Nothing -> randomTrace
    Just t  -> case parseTrace $ TL.toStrict t of
      Left e -> do
        liftAndCatchIO $ putStr "Error: X-Cloud-Trace-Context " >> print
          (traceMaybe, e)
        randomTrace
      Right trace -> incrementTrace trace

mergeAeson :: Value -> Value -> Value
mergeAeson (Object x) (Object y) = Object $ x <> y
mergeAeson _          _          = error "not objects"

data AcceptType
  = TextHTML
  | ApplicationJSON
  deriving (Eq, Show)

getAcceptType :: ActionM AcceptType
getAcceptType =
  Scotty.header "Accept"
    <&> fromMaybe ""
    <&> (TE.encodeUtf8 . TL.toStrict)
    <&> parseHttpAccept
    <&> listToMaybe
    <&> fromMaybe "text/html"
    <&> (\case
          "text/html"        -> TextHTML
          "application/json" -> ApplicationJSON
          _                  -> TextHTML
        )

addIAPHeader :: Application -> Application
addIAPHeader app req respond =
  let headerList = requestHeaders req
      iapEmail =
        TE.encodeUtf8
          $ T.replace "accounts.google.com:" ""
          $ maybe "<Unknown>" TE.decodeUtf8
          $ lookup "X-Goog-Authenticated-User-Email" headerList
  in  addHeaders [("X-Goog-Authenticated-User-Email", iapEmail)] app req respond

createServer :: AppSettings -> IO Application
createServer as = do
  let pool = dbPool as
  templates      <- compileMustacheDir "messagelist.html" (publicDir as)
  cacheContainer <- initCaching NoCaching
  mKey           <- V.newKey

  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = True
        , settingDisableSession               = False
        , settingUseServerName                = False
        }
  manager     <- newManager $ mkManagerSettings tlsSettings Nothing
  accountsEnv <- mkClientEnv manager
    <$> parseBaseUrl "https://accounts-fsm-web.default.svc.cluster.local:443"
  privacyEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://api-privacy-web-internal.default.svc.cluster.local:443"
  payAuthEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://payment-auth-web-internal.default.svc.cluster.local:443"

  scottyApp $ do
    defaultHandler $ \err -> do
      req <- request
      liftAndCatchIO $ putStr "Error at defaultHandler: " >> print
        (err, requestMethod req, rawPathInfo req, requestHeaders req, req)
      text $ "500 error; trace: " <> err

    middleware . gzip $ def { gzipFiles = GzipCompress }
    middleware $ cors
      (const $ Just CorsResourcePolicy
        { corsOrigins        = Nothing
        , corsMethods = ["GET", "HEAD", "POST", "OPTIONS", "PUT", "DELETE"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        , corsExposedHeaders = Just
          ["Authorization", "X-Goog-Authenticated-User-Email"]
        , corsMaxAge         = Nothing
        , corsVaryOrigin     = False
        , corsRequireOrigin  = False
        , corsIgnoreFailures = True
        }
      )
    middleware $ stackDriverMiddleware $ stackDriverInfo as

    middleware addIAPHeader

    -- options (regex ".*") $ status status200 >> text "CORS Ok"

    get "/rewards/list" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        trace <- createTrace
        let fn = _RewardsGetAllEver (accountsRoutes accountsEnv) trace
        boosts <- liftAndCatchIO fn
        Scotty.json boosts

    get "/rewards/reward/:rewardid" $ do
      rid   <- RewardId . fromJust . fromText <$> param "rewardid"
      trace <- createTrace
      let fn = _RewardsGetSpecific (accountsRoutes accountsEnv) trace rid
      boost <- liftAndCatchIO fn

      getAcceptType >>= \case
        TextHTML -> do
          _showOld :: Text <- param "old" `rescue` const next
          trxs <- liftAndCatchIO (withResource pool getMatchTestTransactions)
          html . renderHtml . appShell (Reward boost) $ renderRewardMatch
            boost
            trxs
        ApplicationJSON -> Scotty.json boost

    get "/rewards/reward/:rewardid/purchases" $ do
      rid   <- RewardId . fromJust . fromText <$> param "rewardid"
      trace <- createTrace
      let fn = _RewardsGetSpecific (accountsRoutes accountsEnv) trace rid
      boost <- liftAndCatchIO fn
      trxs  <- liftAndCatchIO (withResource pool getMatchTestTransactions)
      let matches = fmap (\t -> (categorize t boost, t)) trxs

      getAcceptType >>= \case
        TextHTML ->
          html . renderHtml . appShell (Reward boost) $ renderRewardMatch
            boost
            trxs
        ApplicationJSON ->
          Scotty.json $ object ["boost" .= boost, "matches" .= matches]

    get "/rewards/activations/:gid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        gid    <- GroupId . fromJust . fromText <$> param "gid"
        boosts <- liftAndCatchIO
          (withResource pool $ getRewardBoostActivations gid)
        Scotty.json boosts

    get "/rewards/boosts/:gid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        gid   <- GroupId . fromJust . fromText <$> param "gid"
        trace <- createTrace
        let fn = _GroupGetRewards (accountsRoutes accountsEnv) trace gid
        boosts <- liftAndCatchIO fn
        Scotty.json boosts

    post "/rewards/activate" $ do
      trace <- createTrace
      gid   <- GroupId . fromJust . fromText <$> param "groupid"
      rid   <- RewardId . fromJust . fromText <$> param "rewardid"
      uid   <- UserID . fromJust . fromText <$> param "userid"

      newId <- ActivatedRewardId <$> liftAndCatchIO nextRandom

      let fn =
            _GroupActivateReward (accountsRoutes accountsEnv) trace gid
              $ ActivateRewardBody { activatedBy      = uid
                                   , rewardToActivate = rid
                                   , newActivationId  = newId
                                   }

      _ <- liftAndCatchIO fn
      getAcceptType >>= \case
        TextHTML        -> text "Ok"
        ApplicationJSON -> Scotty.json $ object []

    post "/rewards/new" $ do
      trace                    <- createTrace
      newReward :: RewardBoost <- jsonData
      newId                    <- RewardId <$> liftAndCatchIO nextRandom
      now                      <- liftAndCatchIO getCurrentTime
      let fn = _RewardsAddNew (accountsRoutes accountsEnv) trace $ newReward
            { boostId            = newId
            , boostCreated       = now
            , boostUpdated       = now
            , boostMaximumPayout = roundDownUSD $ boostMaximumPayout newReward
            }
      _ <- liftAndCatchIO fn
      text "OK"

    post "/rewards/activation/:rbaid/cancel" $ do
      trace <- createTrace
      rbaid <- ActivatedRewardId . fromJust . fromText <$> param "rbaid"

      let fn = _RewardsCancelSpecific accountsClientM trace rbaid
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Left ce ->
          status status500 >> Scotty.json (object ["error" .= show ce])
        Right _ -> Scotty.json $ object []

    put "/rewards/reward/:rewardid" $ do
      trace     <- createTrace
      textrid   <- param "rewardid"
      newReward <- jsonData
      now       <- liftAndCatchIO getCurrentTime

      let rid      = RewardId . fromJust . fromText $ textrid
      let getBoost = _RewardsGetSpecific (accountsRoutes accountsEnv) trace rid
      boost <- liftAndCatchIO getBoost

      let setBoost =
            _RewardsEditSpecific (accountsRoutes accountsEnv) trace rid $ boost
              { boostMatch         = boostMatch newReward
              , boostUpdated       = now
              , boostActive        = boostActive newReward
              , boostName          = boostName newReward
              , boostRewardInBips  = boostRewardInBips newReward
              , boostExpiresInHr   = boostExpiresInHr newReward
              , boostMaximumPayout = boostMaximumPayout newReward
              , boostUses          = boostUses newReward
              }
      _ <- liftAndCatchIO setBoost

      getAcceptType >>= \case
        TextHTML -> redirect $ "/rewards/reward/" <> TL.fromStrict textrid
        ApplicationJSON -> Scotty.json $ object []

    get "/dashboard" $ do
      users        <- liftAndCatchIO (withResource pool getUsers)
      transactions <- fmap fst
        <$> liftAndCatchIO (withResource pool getAllTransactions)
      let htmlCode = renderDashboard transactions users
      html . renderHtml $ appShell Dashboard htmlCode

    get "/users" $ do
      getAcceptType >>= \case
        TextHTML        -> next
        ApplicationJSON -> do
          setHeader "Cache-Control" "max-age=60"
          uids :: Maybe [UserID] <- (Just . read <$> param "ids")
            `rescue` const (return Nothing)
          vals <- liftAndCatchIO $ withResource pool $ maybe getUsers
                                                             getUsersWithIds
                                                             uids
          Scotty.json vals

    get "/users.csv" $ do
      setHeader "Cache-Control"       "max-age=60"
      setHeader "content-type"        "text/csv"
      setHeader "Content-Disposition" "attachment; filename=\"users.csv\""
      users <- liftAndCatchIO $ withResource pool getUsers

      let decrypter = ssnDecrypt as
      let eCatcher (_ :: SomeException) = return $ PlainText "Error Decrypting"

      rawSSNs <- liftAndCatchIO $ mapM
        (\u -> do
          newSSN <- case usrSSN u of
            Nothing                 -> return Nothing
            Just (RedactedText ssn) -> do
              (PlainText pt) <- decrypter (CipherText ssn) `catch` eCatcher
              return $ Just pt
          return $ u { usrSSN = RedactedText <$> newSSN }
        )
        users
      raw $ encodeDefaultOrderedByName rawSSNs

    get "/users/search/text" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        setHeader "Cache-Control" "max-age=60"
        textStr :: Text <- param "q"
        vals <- liftAndCatchIO . withResource pool $ searchUser textStr
        Scotty.json vals

    get "/users/search/address" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        setHeader "Cache-Control" "max-age=60"
        textStr :: Text <- param "q"
        vals <- liftAndCatchIO . withResource pool $ searchUsersAddress textStr
        Scotty.json vals

    get "/users/search/banking" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        setHeader "Cache-Control" "max-age=60"
        textStr :: Text <- param "q"
        vals <- liftAndCatchIO . withResource pool $ searchUsersBanking textStr
        Scotty.json vals

    get "/ledger/journals" $ do
      extJournals <-
        liftAndCatchIO
        $ withResource pool
        $ getAllJournalsOf
        $ ExternalAccount
        $ DwollaACH ""
      virtualJournals <- liftAndCatchIO $ withResource pool $ getAllJournalsOf
        VirtualAccount

      let htmlCode = renderJournalList extJournals virtualJournals
      getAcceptType >>= \case
        TextHTML        -> html . renderHtml $ appShell LedgerJournal htmlCode
        ApplicationJSON -> Scotty.json $ virtualJournals <> extJournals

    post "/ledger/journals/:jtype" $ do
      name             <- param "name"
      dwollaId         <- DwollaACH <$> param "dwolla"
      jTypeTxt :: Text <- param "jtype"
      trace            <- createTrace

      newId            <- liftAndCatchIO nextRandom

      jType            <- case (jTypeTxt, dwollaId) of
        ("external", DwollaACH "") ->
          status status500 >> text "dwollaId can't be empty" >> finish
        ("external", DwollaACH _) -> return $ ExternalAccount dwollaId
        ("virtual", _) -> return VirtualAccount
        t -> status status500 >> text (TL.pack $ show (t, name)) >> finish

      let body = CreateLedgerJournalBody { newJournalId   = JournalId newId
                                         , newJournalType = jType
                                         , newJournalName = name
                                         , startBalance   = Currency "USD" 0
                                         }

      liftAndCatchIO $ putStr "/ledger/journals/:jtype" >> print
        (name, jTypeTxt, dwollaId, trace, newId, jType, body)

      let fn = _CreateJournal payAuthClientM trace body
      res <- liftAndCatchIO $ runClientM fn payAuthEnv
      case res of
        Right _ -> getAcceptType >>= \case
          TextHTML        -> redirect "/ledger/journals"
          ApplicationJSON -> Scotty.json $ object []
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /ledger/journals/:jtype _CreateJournal "
            >> print (name, jType, e)
          status status500 >> text (TL.pack $ show (name, body, e))

    post "/ledger/search" $ do
      trace            <- createTrace
      jTypeTxt :: Text <- param "jType"
      uid :: Text      <- param "uid"

      let
        srch = case jTypeTxt of
          "PayTgthr"   -> GetPayTgthr $ UserID $ fromJust $ U.fromText uid
          "StashTgthr" -> GetStashTgthr $ UserID $ fromJust $ U.fromText uid
          "SaveTgthr"  -> GetSaveTgthr $ UserID $ fromJust $ U.fromText uid
          "FundingSource" ->
            GetFundingSource $ UserID $ fromJust $ U.fromText uid
          "ExternalAccount" -> GetExternalAccount
          "VirtualAccount" -> GetVirtualAccount
          _ -> error $ "Error: not recognized searcg " <> show (jTypeTxt, uid)

      liftAndCatchIO $ putStr "/ledger/search" >> print (jTypeTxt, uid, srch)

      let fn = _GetJournal payAuthClientM trace srch
      res <- liftAndCatchIO $ runClientM fn payAuthEnv
      case res of
        Right a -> text $ TL.pack $ show a
        Left  e -> do
          liftAndCatchIO $ putStr "Error: /ledger/search _GetJournal " >> print
            (jTypeTxt, uid, srch)
          status status500 >> text (TL.pack $ show (jTypeTxt, uid, srch, e))

    post "/ledger/journals/user/:uid" $ do
      uid              <- fromJust . fromText <$> param "uid"
      jTypeTxt :: Text <- param "jtype"
      dwollaId :: Text <- param "dwolla"
      trace            <- createTrace

      newId            <- JournalId <$> liftAndCatchIO nextRandom

      jType            <- case (jTypeTxt, dwollaId) of
        ("PayTgthr"  , _) -> return $ PayTgthr $ UserID uid
        ("StashTgthr", _) -> return $ StashTgthr $ UserID uid
        ("SaveTgthr" , _) -> return $ SaveTgthr $ UserID uid
        ("FundingSource", "") ->
          status status500
            >> text (TL.pack $ show (jTypeTxt, dwollaId))
            >> finish
        ("FundingSource", t) ->
          return $ FundingSource (UserID uid) (DwollaACH t)
        t -> status status500 >> text (TL.pack $ show t) >> finish

      let body = CreateLedgerJournalBody
            { newJournalId   = newId
            , newJournalType = jType
            , newJournalName = "User's " <> journalTag jType
            , startBalance   = Currency "USD" 0
            }

      liftAndCatchIO $ putStr "/ledger/journals/user/:uid" >> print
        (uid, jTypeTxt, dwollaId, trace, newId, jType, body)

      let fn = _CreateJournal payAuthClientM trace body
      res <- liftAndCatchIO $ runClientM fn payAuthEnv
      case res of
        Right _ ->
          redirect $ "/user/" <> TL.fromStrict (toText uid) <> "#ledger"
        Left e -> do
          let errMsg = (uid, jTypeTxt, dwollaId, e)
          liftAndCatchIO
            $  putStr "Error: /ledger/journals/user/:uid _CreateJournal "
            >> print errMsg
          status status500 >> text (TL.pack $ show errMsg)

    get "/leger/users/owe/us" $ do
      trace <- createTrace
      let fn = _GetUsersNeedToRepayUs payAuthClientM trace
      res <- liftAndCatchIO $ runClientM fn payAuthEnv
      case res of
        Right list -> json list
        Left  e    -> do
          liftAndCatchIO
            $  putStr "Error: /leger/users/owe/us _GetUsersNeedToRepayUs "
            >> print e
          status status500 >> text (TL.pack $ show e)

    get "/ledger/entry/:entryid" $ do
      getAcceptType >>= \case
        TextHTML        -> next
        ApplicationJSON -> do
          entryId <- LedgerEntryId . fromJust . fromText <$> param "entryid"
          res <- liftAndCatchIO $ withResource pool (LDB.getLedgerEntry entryId)
          case res of
            Nothing -> status status404 >> Scotty.json (object [])
            Just le -> Scotty.json $ object ["entry" .= le]

    get "/ledger/transaction/:tid" $ do
      getAcceptType >>= \case
        TextHTML        -> next
        ApplicationJSON -> do
          tid <- LedgerTrxId . fromJust . fromText <$> param "tid"
          res <- liftAndCatchIO
            $ withResource pool (LDB.getLedgerTransaction tid)
          case res of
            Nothing -> status status404 >> Scotty.json (object [])
            Just le -> Scotty.json $ object ["transaction" .= le]

    delete "/ledger/transaction/:tid" $ do
      tid <- LedgerTrxId . fromJust . fromText <$> param "tid"
      res <- liftAndCatchIO
        $ withResource pool (LDB.unsafeDeleteLedgerTransaction tid)
      Scotty.json $ object ["deleted" .= res]

    get "/users/cardcreated/text" $ do
      vals <- liftAndCatchIO $ withResource pool getUsers
      let users = filter (\UserModel {..} -> usrUserState == UserActive) vals
      let userCols = foldr
            (\UserModel {..} acc -> acc <> T.pack
              (  show usrEmail
              <> ","
              <> show usrUserState
              <> ","
              <> show usrCreatedOn
              <> ","
              <> maybe "" show usrCardCreatedOn
              <> ","
              <> maybe "" show usrBecameActiveOn
              <> "\n"
              )
            )
            ""
            users
      text $ TL.fromStrict userCols

    get "/users/setdate" $ do
      srcMsgMaybe <- request <&> vault <&> V.lookup mKey
      let srcMsg =
            tgthrMsgid $ fromMaybe (error "need source message") srcMsgMaybe
      vals <- liftAndCatchIO $ withResource pool getUsers
      liftAndCatchIO $ putStrLn $ "/users/setdate users processing: " <> show
        (length vals)
      let users    = fmap usrUserID vals
      let withPool = withResource pool
      mapM_
        (\(UserID userID) -> liftAndCatchIO $ do
          userRevs <- withPool (Chewpaca.DB.Users.getUser userID)
          putStrLn $ "/users/setdate is happening on " <> show userID

          let createdOn = snd . last $ userRevs
          let firstSignedIn = (snd <$>) . listToMaybe . reverse $ filter
                (\(UserModel {..}, _) -> isJust usrPassword)
                userRevs
          let activated = (snd <$>) . listToMaybe . reverse $ filter
                (\(UserModel {..}, _) -> usrUserState == UserActive)
                userRevs
          let cardCreated = (snd <$>) . listToMaybe . reverse $ filter
                (\(UserModel {..}, _) -> usrAptoCardStatus == Just CardCreated)
                userRevs
          let cardActive = (snd <$>) . listToMaybe . reverse $ filter
                (\(UserModel {..}, _) -> usrAptoCardStatus == Just CardActive)
                userRevs

          case userRevs of
            []                   -> return ()
            (currentUser, _) : _ -> do
              let newUser = currentUser
                    { usrCreatedOn       = createdOn
                    , usrFirstSignIn     = firstSignedIn
                    , usrBecameActiveOn  = activated
                    , usrCardCreatedOn   = cardCreated
                    , usrCardActivatedOn = cardActive
                    , usrRevision        = usrRevision currentUser + 1
                    , usrMsgSource       = srcMsg
                    }
              withPool (saveUser newUser)
        )
        users
      redirect "/users"

    get "/ledger.csv" $ do
      setHeader "Cache-Control"       "max-age=60"
      setHeader "content-type"        "text/csv"
      setHeader "Content-Disposition" "attachment; filename=\"ledger.csv\""
      vals <- liftAndCatchIO $ withResource pool getEntireLedger
      raw $ encodeDefaultOrderedByName vals

    get "/user/:uid/purchases" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getUsersTransactions uid)
        Scotty.json res

    get "/user/:uid/note" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- UserID . fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getUserNote uid)
        Scotty.json $ object ["note" .= res]

    get "/user/:uid/kyc/assesments" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- UserID . fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getUsersKYCAssesments uid)
        Scotty.json res

    get "/user/:uid/codes" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getTokensForUser uid)
        Scotty.json res

    get "/user/:uid/ledger/journals" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- UserID . fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getUserJournals uid)
        Scotty.json res

    get "/user/:uid/ledger/entries" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- UserID . fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getAllEntriesForUser uid)
        Scotty.json res

    get "/user/:uid/risk/score" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO
          $ withResource pool (getCurrentRiskForUser $ UserID uid)
        case res of
          Nothing -> status status404 >> Scotty.json res
          Just r  -> Scotty.json r

    get "/user/:uid/risk/scores" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getRisksForUser uid)
        Scotty.json res

    get "/user/:uid/invites" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- UserID . fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getAllInvitesForUser uid)
        Scotty.json res

    get "/invites" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        res <- liftAndCatchIO $ withResource pool getAllPartnerInvites
        Scotty.json res

    get "/risk/:score/limit" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        setHeader "Cache-Control" "max-age=3600"
        score <- param "score"
        let risk        = exampleRiskScore { rskTrustScore = score }
        let level       = getUserLevelFromScore score
        let limit       = limitByLevel level
        let progrss     = progressToNextLevel risk
        let spendToNext = dollarToNextLevel risk
        Scotty.json $ object
          [ "level" .= level
          , "limit" .= limit
          , "progressToNext" .= progrss
          , "spendToNext" .= spendToNext
          ]

    get "/user/:uid/cards" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- UserID . fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO $ withResource pool (getCardsFor uid)
        Scotty.json res

    get "/card/:cid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- CardId . fromJust . fromText <$> param "cid"
        res <- liftAndCatchIO $ withResource pool (getCardId uid)
        Scotty.json res

    get "/user/:uid/payments" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- UserID . fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO (withResource pool $ getPaymentsForUser uid)
        Scotty.json res

    get "/user/:uid/groups" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- fromJust . fromText <$> param "uid"
        res <- fmap fst
          <$> liftAndCatchIO (withResource pool $ getGroupsForUser uid)
        Scotty.json res

    get "/user/:uid/revisions" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid <- fromJust . fromText <$> param "uid"
        res <- liftAndCatchIO
          $ withResource pool (Chewpaca.DB.Users.getUser uid)
        Scotty.json res

    get "/user/:uid/liability" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid   <- UserID . fromJust . fromText <$> param "uid"
        trace <- createTrace
        let getLiability = _GetLiability payAuthClientM trace uid
        res <- liftAndCatchIO $ runClientM getLiability payAuthEnv
        case res of
          Left ce -> do
            liftAndCatchIO $ putStr "Error: _GetLiability " >> print (uid, ce)
            status status500 >> Scotty.json (object ["error" .= show ce])
          Right cur -> Scotty.json $ object ["liability" .= cur]

    get "/user/:uid/canspend" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        uid   <- UserID . fromJust . fromText <$> param "uid"
        trace <- createTrace
        let canSpend = _GetSpendableBalance payAuthClientM trace uid
        res <- liftAndCatchIO $ runClientM canSpend payAuthEnv
        case res of
          Left ce -> do
            liftAndCatchIO $ putStr "Error: _GetSpendableBalance " >> print
              (uid, ce)
            status status500 >> Scotty.json (object ["error" .= show ce])
          Right cur -> Scotty.json $ object ["canSpend" .= cur]

    get "/user/:uid" $ getAcceptType >>= \case
      ApplicationJSON -> do
        uid  <- UserID . fromJust . fromText <$> param "uid"
        user <- liftAndCatchIO $ withResource pool (getJustUser uid)
        Scotty.json user
      TextHTML -> do
        -- if we add ?old then show this html otherwise react
        _showOld :: Text <- param "old" `rescue` const next

        userID :: UUID   <- fromJust . fromText <$> param "uid"
        let userId = UserID userID
        srcMsg <- request <&> vault <&> V.lookup mKey
        let chan     = fromJust . mqChannel $ as
        let withPool = withResource pool

        let
          extractSpendable :: MessageBody -> Currency
          extractSpendable (ReplyV1 (ReplySuccessV1 (PayReplySuccess GetBalanceReply {..})))
            = gbrBalance
          extractSpendable _ = 0
        let getSpenableCmd =
              (CommandV1 . PayCmd . GetSpendableBalance) (UserID userID)

        let
          extractLibability :: MessageBody -> Currency
          extractLibability (ReplyV1 (ReplySuccessV1 (PayReplySuccess GetUsersLiabilityReply {..})))
            = glrLiability
          extractLibability _ = 0
        let liabilityCmd =
              (CommandV1 . PayCmd . GetUsersLiability) (UserID userID)

        -- User Info Changes (all async)
        modelsA <- liftAndCatchIO . async $ withPool
          (Chewpaca.DB.Users.getUser userID)
        paymentsA <- liftAndCatchIO . async $ withPool
          (getUsersPaymentRevs userId)
        transactionsA <- liftAndCatchIO . async $ withPool
          (getUsersTransactions userID)
        ledgerA <- liftAndCatchIO . async $ withPool (getLedgerForUser userID)
        groupsA <- liftAndCatchIO . async $ withPool (getGroupsForUser userID)
        riskScoresA <- liftAndCatchIO . async $ withPool
          (getRisksForUser userID)
        balancesA <- liftAndCatchIO . async $ withPool
          (getBalancesForUser userID)
        cardsA <- liftAndCatchIO . async $ withPool
          (getCardsFor (UserID userID))

        liabilityResA <- liftAndCatchIO . async $ publishWithReply
          chan
          srcMsg
          liabilityCmd

        spendableReplyA <- liftAndCatchIO . async $ publishWithReply
          chan
          srcMsg
          getSpenableCmd

        tokenA <- liftAndCatchIO . async $ withPool (getTokensForUser userID)
        assesA <- liftAndCatchIO . async $ withPool
          (getUsersKYCAssesments userId)
        notesA       <- liftAndCatchIO . async $ withPool (getUserNote userId)

        journalsA <- liftAndCatchIO . async $ withPool (getUserJournals userId)

        models       <- liftAndCatchIO $ wait modelsA
        payments     <- liftAndCatchIO $ wait paymentsA
        transactions <- liftAndCatchIO $ wait transactionsA
        ledger       <- liftAndCatchIO $ wait ledgerA
        groups       <- liftAndCatchIO $ wait groupsA
        riskScores   <- liftAndCatchIO $ wait riskScoresA
        balances     <- liftAndCatchIO $ wait balancesA
        tokens       <- liftAndCatchIO $ wait tokenA
        cards        <- liftAndCatchIO $ wait cardsA
        asses        <- liftAndCatchIO $ wait assesA
        notes        <- liftAndCatchIO $ wait notesA
        journals     <- liftAndCatchIO $ wait journalsA

        let allGroupUserIds =
              nub
                . fmap (\(UserID u) -> u)
                . concatMap (\(GroupModel {..}, _) -> mbrUser <$> grpMembers)
                $ groups
        allGroupUserWithRevs <- liftAndCatchIO
          $ mapM (withPool . Chewpaca.DB.Users.getUser) allGroupUserIds
        let buildList model@UserModel {..} = (usrUserID, model)
        let allGroupUsers = allGroupUserWithRevs & filter (not . null) & fmap
              (buildList . fst . head)
        case filter null allGroupUserWithRevs of
          [] -> return ()
          gs ->
            liftAndCatchIO
              $  putStr "Error: allGroupUserWithRevs has empty "
              >> print (userID, gs)

        -- Get spendable blalance
        spendableReply <- liftAndCatchIO $ wait spendableReplyA
        let spendable = extractSpendable . tgthrBody . fst $ spendableReply

        -- Get liability of user
        liabilityRes <- liftAndCatchIO $ wait liabilityResA
        let liability = extractLibability . tgthrBody . fst $ liabilityRes

        now <- liftAndCatchIO getCurrentTime

        let userInfo = UserInfo { models         = models
                                , spendable      = spendable
                                , liability      = liability
                                , payments       = payments
                                , transactions   = transactions
                                , ledger         = ledger
                                , groups         = groups
                                , riskScores     = riskScores
                                , balances       = balances
                                , userDB         = allGroupUsers
                                , tokens         = tokens
                                , now            = now
                                , cards          = cards
                                , assessments    = asses
                                , notes          = notes
                                , ledgerJournals = journals
                                }
        let
          lastRev =
            fst . fromMaybe (error "No revs to pull lastRev from") $ listToMaybe
              models
        let fName    = usrFirstName lastRev
        let lName    = usrLastName lastRev
        let fullName = fName <> Just " " <> lName

        html
          . renderHtml
          . appShell (Users (SpecificUser (fullName, UserID userID)))
          $ renderUser userInfo

    get "/user/:uid/ssn" $ do
      userId <- UserID . fromJust . fromText <$> param "uid"
      trace  <- createTrace
      let getUserHttp = _UserGet (accountsRoutes accountsEnv) trace userId
      UserModel {..} <- liftAndCatchIO getUserHttp
      case usrSSN of
        Nothing                 -> text "No SSN for this user"
        Just (RedactedText ssn) -> do
          let decrypter = ssnDecrypt as
          (PlainText pt) <- liftAndCatchIO $ decrypter $ CipherText ssn
          getAcceptType >>= \case
            TextHTML        -> text $ TL.fromStrict pt
            ApplicationJSON -> Scotty.json $ object ["ssn" .= pt]

    get "/user/:uid/kyc/check" $ do
      userId <- UserID . fromJust . fromText <$> param "uid"
      trace  <- createTrace
      let runKYC = _UserRunKYC (accountsRoutes accountsEnv) trace userId
      assesment <- liftAndCatchIO runKYC
      text $ pShowNoColor assesment

    get "/user/:uid/setdate" $ do
      userID :: UUID <- fromJust . fromText <$> param "uid"
      srcMsgMaybe    <- request <&> vault <&> V.lookup mKey
      let srcMsg =
            tgthrMsgid $ fromMaybe (error "need source message") srcMsgMaybe
      let withPool = withResource pool

      userRevs <- liftAndCatchIO $ withPool (Chewpaca.DB.Users.getUser userID)

      let createdOn = snd . last $ userRevs
      let firstSignedIn = (snd <$>) . listToMaybe . reverse $ filter
            (\(UserModel {..}, _) -> isJust usrPassword)
            userRevs
      let activated = (snd <$>) . listToMaybe . reverse $ filter
            (\(UserModel {..}, _) -> usrUserState == UserActive)
            userRevs
      let cardCreated = (snd <$>) . listToMaybe . reverse $ filter
            (\(UserModel {..}, _) -> usrAptoCardStatus == Just CardCreated)
            userRevs
      let cardActive = (snd <$>) . listToMaybe . reverse $ filter
            (\(UserModel {..}, _) -> usrAptoCardStatus == Just CardActive)
            userRevs

      let currentUser = fst . head $ userRevs
      let newUser = currentUser { usrCreatedOn       = createdOn
                                , usrFirstSignIn     = firstSignedIn
                                , usrBecameActiveOn  = activated
                                , usrCardCreatedOn   = cardCreated
                                , usrCardActivatedOn = cardActive
                                , usrRevision = usrRevision currentUser + 1
                                , usrMsgSource       = srcMsg
                                }

      liftAndCatchIO $ withPool (saveUser newUser)

      Web.Scotty.json $ object
        [ "createdOn" .= createdOn
        , "firstSignedIn" .= firstSignedIn
        , "activated" .= activated
        , "cardCreated" .= cardCreated
        , "cardActive" .= cardActive
        ]

    get "/transactions" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        startStr <- (Just <$> param "start") `rescue` const (return Nothing)
        stateStr <- (Just . read <$> param "state")
          `rescue` const (return Nothing)

        let start = startStr >>= iso8601ParseM
        setHeader "Cache-Control" "max-age=600"
        vals <- liftAndCatchIO $ withResource pool $ getTransactions start
                                                                     stateStr
        Scotty.json vals

    get "/transactions.csv" $ do
      setHeader "Cache-Control" "max-age=60"
      setHeader "content-type"  "text/csv"
      setHeader "Content-Disposition"
                "attachment; filename=\"transactions.csv\""
      vals <- liftAndCatchIO $ withResource pool getAllTransactions
      raw $ encodeDefaultOrderedByName $ fmap fst vals

    get "/transactions/stats" $ do
      setHeader "Cache-Control" "max-age=60"
      vals <- liftAndCatchIO $ withResource pool getAllTransactions
      html
        . renderHtml
        . appShell (Transactions TransactionStats)
        . renderTrxStats
        $ fmap fst vals

    get "/payments" $ do
      vals <- liftAndCatchIO $ withResource pool getAllPayments
      getAcceptType >>= \case
        TextHTML -> do
          _showOld :: Text <- param "old" `rescue` const next
          setHeader "Cache-Control" "max-age=60"
          html . renderHtml . appShell (Payments Nothing) $ renderPaymentList
            vals
        ApplicationJSON -> Scotty.json vals

    get "/payments/scheduled" $ do
      getAcceptType >>= \case
        TextHTML        -> next
        ApplicationJSON -> do
          let
            collectPendingPayments (user, balance) = liftAndCatchIO $ do
              payments <- withResource pool (getUsersPendingPayments user)
              let
                normalPayments =
                  filter (\Payment {..} -> paySubType == NormalPayment) payments
              return
                $ object
                    [ "userid" .= user
                    , "ledger" .= balance
                    , "payments" .= normalPayments
                    ]

          usersWithBalance <- liftAndCatchIO
            $ withResource pool LDB.getUsersWithBalances
          withPayments <- mapM collectPendingPayments usersWithBalance

          Scotty.json withPayments

    get "/payment/:pid" $ do
      pid <- PaymentId . fromJust . fromText <$> param "pid"
      getAcceptType >>= \case
        TextHTML -> do
          _showOld :: Text <- param "old" `rescue` const next
          paymentRevisions <-
            liftAndCatchIO $ withResource pool $ getPaymentRevisions pid
          html . renderHtml . appShell (Payments (Just pid)) $ renderPayment
            paymentRevisions
        ApplicationJSON -> do
          payment <- liftAndCatchIO $ withResource pool $ loadPayment pid
          paymentRevisions <-
            liftAndCatchIO $ withResource pool $ getPaymentRevisions pid
          Scotty.json
            $ object ["payment" .= payment, "revisions" .= paymentRevisions]

    post "/payment/:pid/cancel" $ do
      trace <- createTrace
      payId <- fromJust . fromText <$> param "pid"

      let cancel = _CancelPayment payAuthClientM trace $ PaymentId payId
      res <- liftAndCatchIO $ runClientM cancel payAuthEnv
      case res of
        Left  e -> status status500 >> Scotty.json (object ["error" .= show e])
        Right _ -> Scotty.json $ object []

    post "/payment/:pid/state/:newstate" $ do
      trace <- createTrace
      payId <- PaymentId . fromJust . fromText <$> param "pid"
      newState :: PaymentStatus <- read <$> param "newstate"

      let body = UpdatePaymentBody newState Nothing Nothing
      let fn   = _UpdatePayment payAuthClientM trace payId body
      res <- liftAndCatchIO $ runClientM fn payAuthEnv
      case res of
        Left ce -> do
          liftAndCatchIO $ traceError trace "Error: _UpdatePayment " ce
          status status500 >> Scotty.json (object ["error" .= show ce])
        Right _ -> Scotty.json $ object []

    get "/transaction/:id" $ do
      tid  <- TransactionId . fromJust . fromText <$> param "id"
      trxs <- liftAndCatchIO . withResource pool $ getSingleTransactionRevs tid
      getAcceptType >>= \case
        TextHTML -> do
          _showOld :: Text <- param "old" `rescue` const next
          html
            . renderHtml
            . appShell
                (Transactions
                  (SpecificTransaction (tid, trxDescription . fst $ head trxs))
                )
            $ renderTransaction trxs
        ApplicationJSON -> Scotty.json
          $ object ["purchase" .= head (fmap fst trxs), "revisions" .= trxs]

    post "/transaction/:id/set/state/:newstate" $ do
      trace    <- createTrace
      tid      <- TransactionId . fromJust . fromText <$> param "id"
      newState <- fromJust . decode <$> param "newstate"

      let fn = _SetPurchaseStateDirectly payAuthClientM trace tid newState
      res     <- liftAndCatchIO $ runClientM fn payAuthEnv
      accepts <- getAcceptType

      case (res, accepts) of
        (Left e, TextHTML) -> status status500 >> text (TL.pack $ show e)
        (Left e, ApplicationJSON) ->
          status status500 >> Scotty.json (object ["error" .= show e])
        (Right _, TextHTML) -> do
          let unTid (TransactionId t) = TL.fromStrict $ toText t
          redirect $ "/transaction/" <> unTid tid
        (Right _, ApplicationJSON) -> Scotty.json $ object []

    get "/groups" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        setHeader "Cache-Control" "max-age=1800"
        gids :: Maybe [GroupId] <- (Just . read <$> param "ids")
          `rescue` const (return Nothing)
        vals <- liftAndCatchIO $ withResource pool $ maybe getGroups
                                                           getGroupsById
                                                           gids
        Scotty.json vals

    get "/group/new"
      $ (html . snd)
      $ renderMustacheW (templates { templateActual = "newgroup.html" })
      $ object []

    get "/group/:gid" $ do
      gid    <- GroupId . fromJust . fromText <$> param "gid"
      groups <- liftAndCatchIO . withResource pool $ getGroupById gid
      getAcceptType >>= \case
        TextHTML ->
          html . renderHtml . appShell (Groups (Just gid)) $ renderGroup groups
        ApplicationJSON -> Scotty.json $ head $ fmap fst groups

    post "/send/balancerequest" $ do
      uid :: UUID <- fromJust . fromText <$> param "userid"
      srcMsg      <- request <&> vault <&> V.lookup mKey
      msg         <-
        liftAndCatchIO
        . publish (fromJust . mqChannel $ as) srcMsg
        . (CommandV1 . PayCmd . RefreshBalance)
        . UserID
        $ uid
      text $ TL.pack . show . fromMessageID . tgthrMsgid $ msg

    get "/user/:uid/card/:cid/pan" $ do
      uid   <- fromJust . fromText <$> param "uid"
      cid   <- fromJust . fromText <$> param "cid"
      trace <- createTrace
      let getCard = _GetCard accountsClientM trace (CardId cid)

      resGetCard <- liftAndCatchIO $ runClientM getCard accountsEnv
      case resGetCard of
        Right (Just CardModel {..}) -> case cardPlatform of
          AptoPayments   _     -> status status404
          PayWithPrivacy token -> do
            let getUILink =
                  _CardHostedUI pricacyClientM trace (UserID uid) token
            resGetLink <- liftAndCatchIO $ runClientM getUILink privacyEnv
            case resGetLink of
              Right t -> redirect $ TL.fromStrict t
              Left  e -> do
                liftAndCatchIO
                  $  putStr
                       "Error: /user/:uid/privacy/sendto _UserSendToPrivacy "
                  >> print (cid, e)
                status status500 >> text (TL.pack $ show (uid, e))

        Right Nothing -> do
          liftAndCatchIO
            $  putStr
                 "Error: /user/:uid/privacy/sendto _UserSendToPrivacy Nothing "
            >> print cid
          status status500 >> text
            (  TL.pack
            $  show uid
            <> " Error: /user/:uid/privacy/sendto _UserSendToPrivacy Nothing"
            )

        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/privacy/sendto _UserSendToPrivacy "
            >> print (cid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    get "/user/:uid/sync/segment" $ do
      uid <- fromJust . fromText <$> param "uid"
      let fn = _AdminSyncUserSegment accountsClientM (UserID uid)
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/sync/segment _AdminSyncUserSegment "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))


    post "/user/:uid/card/:cid/state/:state" $ do
      uid                    <- fromJust . fromText <$> param "uid"
      cid                    <- CardId . fromJust . fromText <$> param "cid"
      requestedState :: Text <- param "state"
      trace                  <- createTrace

      newState               <- case requestedState of
        "lock"        -> return CardUserFrozen
        "adminlock"   -> return CardAdminFrozen
        "unlock"      -> return CardActive
        "close"       -> return CardClosed
        "ACTIVATED"   -> return CardActive
        "CLOSED"      -> return CardClosed
        "USERFROZEN"  -> return CardUserFrozen
        "ADMINFROZEN" -> return CardAdminFrozen
        "CREATED"     -> text "Not allowed state" >> status status500 >> finish
        _             -> text "Unknown state" >> status status500 >> finish

      let fn =
            _AdminSetCardState accountsClientM trace (UserID uid) cid newState
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/kyc/passed _UserSetKYCState "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/note" $ do
      uid          <- fromJust . fromText <$> param "uid"
      note :: Text <- param "note"
      trace        <- createTrace
      let fn = _AdminRecordUserNote accountsClientM trace (UserID uid) note
      res        <- liftAndCatchIO $ runClientM fn accountsEnv
      acceptType <- getAcceptType
      case (res, acceptType) of
        (Right _, TextHTML) ->
          redirect $ "/user/" <> TL.fromStrict (toText uid) <> "#notes"
        (Right _, ApplicationJSON) -> Scotty.json $ object []
        (Left  e, t              ) -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/notes _AdminRecordUserNote "
            >> print (uid, e)
          case t of
            TextHTML -> status status500 >> text (TL.pack $ show (uid, e))
            ApplicationJSON -> status status500 >> Scotty.json (object [])

    post "/user/:uid/note/append" $ do
      uid          <- UserID . fromJust . fromText <$> param "uid"
      note :: Text <- param "note"
      trace        <- createTrace
      email        <- Scotty.header "X-Goog-Authenticated-User-Email"

      now          <- liftAndCatchIO getCurrentTime
      let timeShown = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" now
      let normalizedEmail =
            T.replace "accounts.google.com:" "" $ TL.toStrict $ fromMaybe
              "<Unknown>"
              email

      currentNote <- liftAndCatchIO $ withResource pool (getUserNote uid)
      let newNote =
            (timeShown <> " " <> normalizedEmail <> "\n")
              <> (note <> "\n")
              <> "--- --- --- \n\n"
              <> currentNote
      let fn = _AdminRecordUserNote accountsClientM trace uid newNote
      res        <- liftAndCatchIO $ runClientM fn accountsEnv
      acceptType <- getAcceptType
      case (res, acceptType) of
        (Right _, TextHTML) -> do
          let fromUID (UserID u) = u
          redirect
            $  "/user/"
            <> TL.fromStrict (toText $ fromUID uid)
            <> "#notes"
        (Right _, ApplicationJSON) -> Scotty.json $ object []
        (Left  e, t              ) -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/notes _AdminRecordUserNote "
            >> print (uid, e)
          case t of
            TextHTML -> status status500 >> text (TL.pack $ show (uid, e))
            ApplicationJSON -> status status500 >> Scotty.json (object [])


    post "/user/:uid/kyc/passed" $ do
      uid   <- fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn = _UserSetKYCState accountsClientM trace (UserID uid)
            $ ChangeKYCStateBody Passed
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/kyc/passed _UserSetKYCState "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/kyc/rejected" $ do
      uid   <- fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn =
            _UserSetKYCState accountsClientM trace (UserID uid)
              $ ChangeKYCStateBody
              $ Rejected [DocumentationIncorrect]
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/kyc/passed _UserSetKYCState "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/kyc/autoverifyfailed" $ do
      uid   <- fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn =
            _UserSetKYCState accountsClientM trace (UserID uid)
              $ ChangeKYCStateBody
              $ AutoVerifyFailed []
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/kyc/passed _UserSetKYCState "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/privacy/sendto" $ do
      uid   <- fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn = _UserSendToPrivacy accountsClientM trace (UserID uid)

      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/privacy/sendto _UserSendToPrivacy "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/dwolla/sendto" $ do
      uid   <- fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn = _UserSendToDwolla accountsClientM trace (UserID uid)

      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/dwolla/sendto _UserSendToDwolla "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/fundingsource/remove" $ do
      uid   <- fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn =
            _UserRemoveFS accountsClientM trace (UserID uid)
              $ RemoveBankFSBody DwollaSettlement Nothing

      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right NoContent -> do
          liftAndCatchIO $ threadDelay 200000
          redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/fundingsource/remove "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/change/fundingsource/bank" $ do
      uid :: UUID   <- fromJust . fromText <$> param "uid"
      bankName      <- param "bankname"
      accountName   <- param "accountname"
      routingNumber <- param "routingnumber"
      accountNumber <- param "accountnumber"

      let body = SetManualFSBody
            { unverifiedABARouting  = RedactedText routingNumber
            , unverifiedDDAAccount  = RedactedText accountNumber
            , unverifiedAccountName = accountName
            , unverifiedBankName    = bankName
            }

      trace <- createTrace
      let changeBank = _SetFSBankManual payAuthClientM trace (UserID uid) body

      res <- liftAndCatchIO $ runClientM changeBank payAuthEnv
      case res of
        Right NoContent -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e         -> status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/fundingsource/verify" $ do
      uid :: UUID <- fromJust . fromText <$> param "uid"
      trace       <- createTrace

      let fn = _UserVerifyCurrentFS accountsClientM trace (UserID uid)
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e -> do
          liftAndCatchIO
            $  putStr
                 "Error: /user/:uid/fundingsource/verify _UserVerifyCurrentFS "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/payment/create/ledgerbalance" $ do
      trace       <- createTrace
      uid :: UUID <- fromJust . fromText <$> param "uid"

      let action = _PayoutLedger payAuthClientM trace $ UserID uid
      res     <- liftAndCatchIO $ runClientM action payAuthEnv
      accepts <- getAcceptType

      case (res, accepts) of
        (Left e, TextHTML) -> status status500 >> text (TL.pack $ show e)
        (Left e, ApplicationJSON) ->
          status status500 >> Scotty.json (object ["error" .= show e])
        (Right _, TextHTML) ->
          redirect $ "/user/" <> TL.fromStrict (toText uid) <> "#payments"
        (Right _, ApplicationJSON) -> Scotty.json $ object []

    post "/user/:uid/token/create/:medium" $ do
      uid :: UUID        <- fromJust . fromText <$> param "uid"
      mediumText :: Text <- param "medium"
      trace              <- createTrace
      tokenId            <- liftAndCatchIO nextRandom

      medium             <- case mediumText of
        "email" -> return EmailMedium
        "phone" -> return PhoneMedium
        "push"  -> return PushMedium
        _       -> text "not a good medium" >> status status500 >> finish

      let fn = _TokenCreate accountsClientM
                            trace
                            (UserID uid)
                            medium
                            TenMinutes
                            (TokenId tokenId)
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right NoContent -> redirect $ "/user/" <> TL.fromStrict (toText uid)
        Left  e         -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/token/create/:medium _TokenCreate "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/token/verify/:medium" $ do
      uid :: UUID        <- fromJust . fromText <$> param "uid"
      mediumText :: Text <- param "medium"
      token              <- param "token"

      medium             <- case mediumText of
        "email" -> return EmailMedium
        "phone" -> return PhoneMedium
        "push"  -> return PushMedium
        _       -> text "not a good medium" >> status status500 >> finish

      trace <- createTrace
      let fn = _TokenVerify accountsClientM trace (UserID uid) medium token
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Right b -> text $ TL.fromStrict $ T.pack $ show b
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/token/verify/:medium _TokenVerify "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/send/runledgergc" $ do
      trace <- createTrace

      let action = _AdminLedgerToPayments payAuthClientM trace
      res     <- liftAndCatchIO $ runClientM action payAuthEnv
      accepts <- getAcceptType
      case (res, accepts) of
        (Left e, TextHTML) -> status status500 >> text (TL.pack $ show e)
        (Left e, ApplicationJSON) ->
          status status500 >> Scotty.json (object ["error" .= show e])
        (Right _, TextHTML       ) -> redirect . TL.fromStrict $ "/"
        (Right _, ApplicationJSON) -> Scotty.json $ object []

    post "/send/runactivebalances" $ do
      trace <- createTrace
      let fn = _AdminRefreshPlaidBalances payAuthClientM trace
      res <- liftAndCatchIO $ runClientM fn payAuthEnv
      case res of
        Left e -> do
          liftAndCatchIO $ putStr "Error: /send/runactivebalances " >> print e
          status status500 >> text (TL.pack $ show e)
        Right NoContent -> text "Ok"

    post "/user/:uid/ledger/adjustment" $ do
      trace    <- createTrace
      uid      <- fromJust . fromText <$> param "uid"
      fromJ    <- JournalId . fromJust . fromText <$> param "from"
      toJ      <- JournalId . fromJust . fromText <$> param "to"
      valueRaw <- param "amount" :: ActionM Double

      let integered = truncate $ valueRaw * 100
      let value     = Currency "USD" (integered % 100)

      let body = CreateLedgerTrxBody { fromJournal = fromJ
                                     , toJournal   = toJ
                                     , fact        = Manual value
                                     }

      let fn = _CreateJournalTrx payAuthClientM trace body
      res     <- liftAndCatchIO $ runClientM fn payAuthEnv
      accepts <- getAcceptType
      case (res, accepts) of
        (Left e, TextHTML) -> do
          traceError trace "Error: /user/:uid/ledger/adjustment " e
          status status500
            >> text (TL.pack $ show (uid, fromJ, toJ, valueRaw, body, e))
            >> finish
        (Left e, ApplicationJSON) -> do
          traceError trace "Error: /user/:uid/ledger/adjustment " e
          status status500 >> Scotty.json (object ["error" .= show e])
        (Right _, TextHTML) ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#ledger"
        (Right _, ApplicationJSON) -> Scotty.json (object [])

    post "/user/:uid/fundingsource/verification/resend" $ do
      uid   <- fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn = _MakeVerificationPayment payAuthClientM trace (UserID uid)
            $ MakeVerificationPaymentBody UserSavedAmounts

      res <- liftAndCatchIO $ runClientM fn payAuthEnv
      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#payments"
        Left e -> do
          liftAndCatchIO
            $  putStr
                 "Error: /user/:uid/fundingsource/verification/resend _MakeVerificationPayment "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/commands/sendstatement" $ do
      srcMsg <- request <&> vault <&> V.lookup mKey
      uid    <- fromJust . fromText <$> param "uid"

      _      <-
        liftAndCatchIO
        . publish (fromJust . mqChannel $ as) srcMsg
        . (EventV1 . PayEvt)
        $ StatementRequested (UserID uid) Nothing (Just 12)

      redirect $ "/user/" <> (TL.pack . show) uid <> "#top"

    post "/group/:gid/resetsplit" $ do
      trace   <- createTrace
      gid     <- fromJust . fromText <$> param "gid"
      groups' <- liftAndCatchIO . withResource pool $ getGroupById (GroupId gid)
      let mostRecent = fst $ head groups'
      let currSplit  = grpSplit mostRecent
      let userCount  = toInteger $ length currSplit
      let newSplit = fmap
            (\s -> s { splRatio = 100 % userCount, splApproved = True })
            currSplit

      let fn = _GroupForceSplit accountsClientM trace (GroupId gid) newSplit
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/group/" <> (TL.pack . show) gid
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /group/:gid/resetsplit _GroupForceSplit "
            >> print (gid, e)
          status status500 >> text (TL.pack $ show (gid, e))

    post "/group/:gid/close" $ do
      gid <- fromJust . fromText <$> param "gid"
      let nullUid = UserID U.nil
      trace <- createTrace

      let
        fn = _GroupClose accountsClientM trace (GroupId gid)
          $ CloseGroupBody nullUid
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/group/" <> (TL.pack . show) gid
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /group/:gid/close _GroupClose "
            >> print (gid, e)
          status status500 >> text (TL.pack $ show (gid, e))

    post "/group/:gid/acceptmember/:uid" $ do
      trace <- createTrace
      gid   <- fromJust . fromText <$> param "gid"
      uid   <- fromJust . fromText <$> param "uid"

      let fn = _GroupApproveInvite accountsClientM
                                   trace
                                   (GroupId gid)
                                   (UserID uid)
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/group/" <> (TL.pack . show) gid
        Left  e -> do
          liftAndCatchIO
            $ putStr "Error: /group/:gid/acceptmember/:uid _GroupApproveInvite "
            >> print (gid, uid, e)
          status status500 >> text (TL.pack $ show (gid, e))

    post "/group/:gid/approvesplit/:uid" $ do
      trace <- createTrace
      gid   <- fromJust . fromText <$> param "gid"
      uid   <- fromJust . fromText <$> param "uid"

      let fn = _GroupApproveSplit accountsClientM
                                  trace
                                  (GroupId gid)
                                  (UserID uid)
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right _ -> redirect $ "/group/" <> (TL.pack . show) gid
        Left  e -> do
          liftAndCatchIO
            $  putStr "Error: /group/:gid/approvesplit/:uid _GroupApproveSplit "
            >> print (gid, uid, e)
          status status500 >> text (TL.pack $ show (gid, e))

    post "/group/new" $ do
      trace  <- createTrace
      newGid <- GroupId <$> liftAndCatchIO nextRandom
      let nullUid = UserID U.nil
      email1         <- EmailAddress <$> param "email1"
      fname1 :: Text <- param "fname1"
      user1          <- UserID <$> liftAndCatchIO nextRandom
      email2         <- EmailAddress <$> param "email2"
      fname2 :: Text <- param "fname2"
      user2          <- UserID <$> liftAndCatchIO nextRandom

      liftAndCatchIO $ print (newGid, (email1, fname1, user1), (email2, fname2))

      let body = CreateGroupBody
            { members      = [(fname1, email1, user1), (fname2, email2, user2)]
            , inviter      = nullUid
            , validThrough = Nothing
            }
      let fn = _GroupCreate accountsClientM trace newGid body

      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right GroupModel {..} -> redirect $ "/group/" <> (TL.pack . show) grpId
        Left  e               -> do
          liftAndCatchIO
            $  putStr "Error: /group/:gid/approvesplit/:uid _GroupApproveSplit "
            >> print (newGid, body, e)
          status status500 >> text (TL.pack $ show (newGid, body, e))

    post "/user/:uid/risk/adjustment" $ do
      srcMsg <- request <&> vault <&> V.lookup mKey
      uid    <- fromJust . fromText <$> param "uid"
      score  <- param "trustscore" :: ActionM Double
      _      <-
        liftAndCatchIO
        . publish (fromJust . mqChannel $ as) srcMsg
        . (CommandV1 . PayCmd)
        $ AdjustUsersRiskScore (UserID uid) (ManualRiskAdj score)
      redirect $ "/user/" <> (TL.pack . show) uid <> "#riskscores"

    post "/user/:uid/state/close/:reason" $ do
      trace            <- createTrace
      uid              <- fromJust . fromText <$> param "uid"
      reason :: String <- param "reason"

      closeReason      <- case reason of
        "fraudy"    -> return FraudyUser
        "overdue"   -> return OverdueBalance
        "duplicate" -> return DuplicateUser
        "requested" -> return ClosedByUser
        _           -> return $ read reason

      let
        fn = _UserClose accountsClientM trace (UserID uid)
          $ CloseUserBody closeReason
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right NoContent -> getAcceptType >>= \case
          TextHTML -> redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
          ApplicationJSON -> Scotty.json $ object []
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: user/:uid/state/close/:reason _UserClose "
            >> print (uid, closeReason, e)
          status status500 >> getAcceptType >>= \case
            TextHTML        -> text (TL.pack $ show (uid, closeReason, e))
            ApplicationJSON -> Scotty.json $ object ["error" .= show e]


    post "/user/:uid/change/email" $ do
      uid   <- fromJust . fromText <$> param "uid"
      email <- normalizeEmail . EmailAddress <$> param "email"

      trace <- createTrace
      let fn = _UserChangeEmail accountsClientM trace (UserID uid) email
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/email _UserChangeEmail "
            >> print (uid, email, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/change/ssn" $ do
      uid    <- fromJust . fromText <$> param "uid"
      rawSSN <- T.filter (/= '-') <$> param "ssn"

      when (rawSSN == "")
           (text "Error: SSN can't be empty" >> status status400 >> finish)

      (CipherText encrypted) <- liftAndCatchIO $ ssnEncrypt as $ PlainText
        rawSSN

      trace <- createTrace
      let fn = _UserUpdate accountsClientM trace (UserID uid)
            $ UpdateUserBody [(EncryptedSSN, Just encrypted)]
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/ssn _UserUpdate "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/change/name" $ do
      uid   <- fromJust . fromText <$> param "uid"
      fName <- param "fname"
      lName <- param "lname"

      let changes =
            UpdateUserBody
              $  [ (NameFirst, Just fName) | fName /= "" ]
              <> [ (NameLast, Just lName) | lName /= "" ]

      trace <- createTrace
      let fn = _UserUpdate accountsClientM trace (UserID uid) changes
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/name _UserUpdate "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/change/dob" $ do
      uid <- fromJust . fromText <$> param "uid"
      dob <- param "dob" -- "MM/DD/YYYY"

      let changes = UpdateUserBody [(DateOfBirth, Just dob)]

      trace <- createTrace
      let fn = _UserUpdate accountsClientM trace (UserID uid) changes
      res     <- liftAndCatchIO $ runClientM fn accountsEnv

      accepts <- getAcceptType
      case (res, accepts) of
        (Right NoContent, TextHTML) ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
        (Right NoContent, ApplicationJSON) -> Scotty.json $ object []
        (Left  e        , TextHTML       ) -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/dob _UserUpdate "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))
        (Left e, ApplicationJSON) -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/dob _UserUpdate "
            >> print (uid, e)
          status status500 >> Scotty.json (object ["error" .= show e])

    post "/user/:uid/change/dwolla" $ do
      uid        <- fromJust . fromText <$> param "uid"
      dwollaId   <- param "dwollaid"
      dwollaFsId <- param "dwollafsid"

      let changes =
            [ (DwollaCustomerId, Just dwollaId) | dwollaId /= "" ]
            <> [ (DwollaFundingId, Just dwollaFsId) | dwollaFsId /= "" ]

      trace <- createTrace
      let
        fn = _UserUpdate accountsClientM trace (UserID uid)
          $ UpdateUserBody changes
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/name _UserUpdate "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/change/address" $ do
      uid                <- fromJust . fromText <$> param "uid"
      firstLine :: Text  <- T.strip <$> param "firstline"
      secondLine :: Text <- T.strip <$> param "secondline"
      city :: Text       <- T.strip <$> param "city"
      state :: Text      <- T.strip <$> param "state"
      zipcode :: Text    <- T.strip <$> param "zip"

      when (T.length firstLine == 0)
           (text "firstline is 0 length" >> status status400 >> finish)
      when (T.length city == 0)
           (text "city is 0 length" >> status status400 >> finish)
      when
        (T.length state /= 2)
        (text "state needs to be 2 characters" >> status status400 >> finish)
      when (T.length zipcode /= 5)
           (text "zip needs to be 5 characters" >> status status400 >> finish)

      trace <- createTrace
      let changes = UpdateUserBody
            [ (AddressStreet, Just firstLine)
            , ( AddressStreet2
              , if T.length secondLine == 0 then Nothing else Just secondLine
              )
            , (AddressCity , Just city)
            , (AddressState, Just state)
            , (AddressZip  , Just zipcode)
            ]

      let fn = _UserUpdate accountsClientM trace (UserID uid) changes
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#revisions"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/address  _UserUpdate "
            >> print (uid, changes, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/change/phone" $ do
      uid                 <- fromJust . fromText <$> param "uid"
      phoneNumber :: Text <- param "phone"

      when
        (T.length phoneNumber /= 10)
        (  text "phone must be 10 numbers 1231231234"
        >> status status400
        >> finish
        )

      let changes = UpdateUserBody [(Phone, Just phoneNumber)]
      trace <- createTrace
      let fn = _UserUpdate accountsClientM trace (UserID uid) changes
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#revisions"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/change/phone _UserUpdate "
            >> print (uid, changes, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/state/activate" $ do
      uid   <- fromJust . fromText <$> param "uid"

      trace <- createTrace
      let fn = _UserForceState accountsClientM trace (UserID uid) UserActive
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/state/activate _UserForceState "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    post "/user/:uid/card/create/:type" $ do
      uid   <- fromJust . fromText <$> param "uid"
      cType <- read <$> param "type"

      trace <- createTrace
      let fn = _UserCreateCard accountsClientM trace (UserID uid) cType
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right NoContent ->
          redirect $ "/user/" <> (TL.pack . show) uid <> "#top"
        Left e -> do
          liftAndCatchIO
            $  putStr "Error: /user/:uid/card/create _UserCreateCard "
            >> print (uid, e)
          status status500 >> text (TL.pack $ show (uid, e))

    get "/api/user/fromemail/:email" $ do
      trace <- createTrace
      email <- EmailAddress <$> param "email"

      setHeader "content-type" "application/json" -- required for GZIP
      let fn = _UsersQueryEmail accountsClientM trace email
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Right (usr : _) -> Web.Scotty.json usr
        Right []        -> status status404 >> Web.Scotty.json (object [])
        Left  e         -> do
          liftAndCatchIO
            $  putStr "Error: /api/user/fromemail/:email _UsersQueryEmail "
            >> print (email, e)
          status status500
            >> Web.Scotty.json (object ["email" .= email, "error" .= show e])

    get "/api/user/:uid" $ do
      uid   <- UserID . fromJust . fromText <$> param "uid"
      trace <- createTrace

      let getUserHttp = _UserGet (accountsRoutes accountsEnv) trace uid
      u <- liftAndCatchIO getUserHttp

      setHeader "content-type" "application/json" -- required for GZIP
      Web.Scotty.json u

    get "/api/user/:uid/group" $ do
      srcMsg <- request <&> vault <&> V.lookup mKey
      uid    <- UserID . fromJust . fromText <$> param "uid"

      let pub cmd =
            publishWithReply (fromJust . mqChannel $ as) srcMsg (CommandV1 cmd)
              <&> fst
              <&> tgthrBody
      group <- liftAndCatchIO $ getGroupForUser pub uid

      setHeader "content-type" "application/json" -- required for GZIP
      case group of
        Left  _ -> status status404 >> text ""
        Right g -> Web.Scotty.json g

    get "/reports/weekly/global" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        fullData <- liftAndCatchIO $ generateWeekly pool
        Scotty.json fullData

    get "/reports/:interval/user/:uid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        user                     <- UserID . fromJust . fromText <$> param "uid"
        selectedInterval :: Text <- param "interval"
        interval                 <- case selectedInterval of
          "daily"   -> return "1 day"
          "weekly"  -> return "7 day"
          "monthly" -> return "1 month"
          _         -> next

        fullData <- liftAndCatchIO $ generateWeeklyUser interval user pool
        Scotty.json fullData

    post "/reports/:interval/user/:uid" $ Scotty.json $ object []

    post "/reports/:interval/users" $ Scotty.json $ object []

    get "/reports/rewards/:interval/users/" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        selectedInterval :: Text <- param "interval"
        interval                 <- case selectedInterval of
          "daily"   -> return "1 day"
          "weekly"  -> return "7 day"
          "monthly" -> return "1 month"
          _         -> next

        fullData <- liftAndCatchIO $ generateEarnedRewards interval pool
        Scotty.json fullData

    get "/users/today" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        fullData <- liftAndCatchIO $ signedUpLast24Hours pool
        Scotty.json fullData

    get "/appevents/user/:uid/" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        user <- UserID . fromJust . fromText <$> param "uid"
        liftAndCatchIO (getAppEventsForUser user pool) >>= Scotty.json

    get "/appevents/devices" $ getAcceptType >>= \case
      TextHTML -> next
      ApplicationJSON ->
        liftAndCatchIO (getDevicesWithAppEvents pool) >>= Scotty.json

    get "/appevents/device/:deviceid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        device <- param "deviceid"
        liftAndCatchIO (getAppEventsForDevice device pool) >>= Scotty.json

    get "/appevents/users" $ getAcceptType >>= \case
      TextHTML -> next
      ApplicationJSON ->
        liftAndCatchIO (getUsersWithAppEvents pool) >>= Scotty.json

    get "/ledger/journal/:jid/entries" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        jourId <- JournalId . fromJust . fromText <$> param "jid"
        res <- liftAndCatchIO $ withResource pool (getEntriesForJournal jourId)
        Scotty.json res

    get "/ledger/journal/:jid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        jourId <- JournalId . fromJust . fromText <$> param "jid"
        res    <- liftAndCatchIO $ withResource pool (getJournal jourId)
        case res of
          Nothing -> status status404 >> Scotty.json (object [])
          Just lj -> Scotty.json lj

    get "/referrals/programs" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        res <- liftAndCatchIO $ withResource pool getAllReferralPrograms
        Scotty.json res

    get "/referrals/programs/:pid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        pid <- ReferralProgramID . fromJust . fromText <$> param "pid"
        res <- liftAndCatchIO $ withResource pool (getReferralProgram pid)
        Scotty.json $ object ["program" .= res]

    get "/referrals/codes/user/:uid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        user <- UserID . fromJust . fromText <$> param "uid"
        res  <- liftAndCatchIO $ withResource pool (getReferralCodeForUser user)
        Scotty.json $ object ["code" .= res]

    get "/referrals/codes/:code" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        theCode <- param "code"
        res     <- liftAndCatchIO $ withResource pool (getReferralCode theCode)
        Scotty.json $ object ["code" .= res]

    get "/referrals/codes" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        res <- liftAndCatchIO $ withResource pool getAllReferralCodes
        Scotty.json res

    get "/referrals/progress/self/:uid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        user <- UserID . fromJust . fromText <$> param "uid"
        res  <- liftAndCatchIO $ withResource pool (getReferralProgressOf user)
        Scotty.json $ object ["progress" .= res]

    get "/referrals/progress/referee/:uid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        user <- UserID . fromJust . fromText <$> param "uid"
        res  <- liftAndCatchIO $ withResource pool (getReferreeProgressFor user)
        Scotty.json res

    get "/referrals/progress/revisions/:pid" $ getAcceptType >>= \case
      TextHTML        -> next
      ApplicationJSON -> do
        pid <- ReferralProgressID . fromJust . fromText <$> param "pid"
        res <- liftAndCatchIO
          $ withResource pool (getReferralProgressRevisions pid)
        Scotty.json res

    post "/referrals/new/program" $ do
      program :: ReferralProgram <- jsonData
      trace                      <- createTrace
      accepts                    <- getAcceptType

      newId                      <- liftAndCatchIO nextRandom
      now                        <- liftAndCatchIO getCurrentTime
      let normalized = program { refProgram   = ReferralProgramID newId
                               , refCreatedAt = now
                               , refUpdatedAt = now
                               , refRevision  = 1
                               }
      let fn = _SetReferralProgram accountsClientM trace normalized
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case (res, accepts) of
        (Left e, TextHTML) -> text . TL.pack $ show e
        (Left e, ApplicationJSON) ->
          status status500 >> Scotty.json (object ["error" .= show e])
        (Right _, TextHTML       ) -> next
        (Right _, ApplicationJSON) -> Scotty.json $ object []

    post "/referrals/programs/:pid" $ do
      program :: ReferralProgram <- jsonData
      trace                      <- createTrace

      now                        <- liftAndCatchIO getCurrentTime
      let normalized = program { refUpdatedAt = now }
      let fn         = _SetReferralProgram accountsClientM trace normalized
      res <- liftAndCatchIO $ runClientM fn accountsEnv

      case res of
        Left  e -> status status500 >> Scotty.json (object ["error" .= show e])
        Right _ -> Scotty.json $ object []

    post "/referrals/new/code/:uid" $ do
      user  <- UserID . fromJust . fromText <$> param "uid"
      trace <- createTrace
      let fn = _GetRererralCode accountsClientM trace user
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Left ce ->
          status status500 >> Scotty.json (object ["error" .= show ce])
        Right rc -> Scotty.json $ object ["code" .= rc]

    post "/referrals/codes/:code/link/:uid" $ do
      code  <- param "code"
      user  <- UserID . fromJust . fromText <$> param "uid"
      trace <- createTrace

      let fn = _UseReferralCode accountsClientM trace code user
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Left ce ->
          status status500 >> Scotty.json (object ["error" .= show ce])
        Right _ -> Scotty.json $ object []

    get "/referrals/progress" $ do
      getAcceptType >>= \case
        TextHTML        -> next
        ApplicationJSON -> do
          res <- liftAndCatchIO $ withResource pool getAllReferralProgress
          Scotty.json res

    post "/referrals/progress/:pid" $ do
      progress :: ReferralProgress <- jsonData
      trace                        <- createTrace

      let fn = _SetReferralProgress accountsClientM trace progress
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Left ce ->
          status status500 >> Scotty.json (object ["error" .= show ce])
        Right _ -> Scotty.json $ object []

    put "/referrals/progress/:uid" $ do
      user <- UserID . fromJust . fromText <$> param "uid"
      progress :: WorkFlowProgress <- jsonData
      trace <- createTrace

      let fn = _UpdateReferralProgress accountsClientM trace user progress
      res <- liftAndCatchIO $ runClientM fn accountsEnv
      case res of
        Left ce ->
          status status500 >> Scotty.json (object ["error" .= show ce])
        Right _ -> Scotty.json $ object []

    middleware $ staticPolicyWithOptions
      (defaultOptions { cacheContainer = cacheContainer })
      (noDots >-> addBase "public")

    get (regex ".*") $ getAcceptType >>= \case
      TextHTML -> do
        setHeader "content-type" "text/html; charset=UTF-8" -- required for GZIP
        status status200
        file "public/index.html"
      ApplicationJSON -> next

    notFound $ getAcceptType >>= \case
      TextHTML        -> text "Not found"
      ApplicationJSON -> Scotty.json $ object []

startServer :: AppSettings -> IO ()
startServer tgthrSettings = do
  let excHandler :: SomeException -> Wai.Response
      excHandler se =
        responseLBS status500 [(hContentType, "text/plain; charset=utf-8")]
          . (BL.fromStrict . S8.pack)
          $ ("Error encountered: " <> show se)

  let appSettings =
        defaultSettings
          & setPort (port tgthrSettings)
          & setOnExceptionResponse excHandler

  waiApp      <- createServer tgthrSettings

  jsonLogging <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                       formatAsJSONWithHeaders
    }

  putStr "Server running at http://localhost:" >> print (getPort appSettings)
  runSettings appSettings . jsonLogging $ waiApp
