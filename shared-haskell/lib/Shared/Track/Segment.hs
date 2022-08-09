{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}
module Shared.Track.Segment
  ( trackGroup
  , sendUserBalance
  , trackOneOffTrait
  , trackEvent
  , trackTransactionUpdated
  , sendUserUpdate
  , createSegmentRequestor
  , CurrentUser(..)
  , PayeeUser(..)
  ) where

import           Control.Exception              ( SomeException )
import           Control.Monad                  ( void )
import           Data.Aeson                     ( KeyValue(..)
                                                , Value(..)
                                                , encode
                                                , object
                                                )
import qualified Data.ByteString.Char8         as BC
import           Data.Foldable                  ( asum )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import           Network.HTTP.Client            ( Request
                                                  ( method
                                                  , requestBody
                                                  , requestHeaders
                                                  )
                                                , RequestBody(RequestBodyLBS)
                                                , applyBasicAuth
                                                , httpLbs
                                                , parseRequest
                                                )
import           Network.HTTP.Client.TLS        ( newTlsManager )
import           Network.HTTP.Types.Method      ( StdMethod(POST)
                                                , renderStdMethod
                                                )
import           Shared.Models.Apto.Base        ( AptoCardholderId
                                                  ( AptoCardholderId
                                                  )
                                                )
import           Shared.Models.CategorySplit    ( categoryDescription )
import           Shared.Models.Currency         ( Currency
                                                , getMonetaryValue
                                                )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.KYC              ( KYCFailureReasons(..)
                                                , KycStatus(..)
                                                , getFailureReasons
                                                )
import           Shared.Models.Transaction      ( DeclineReason(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( PhoneNumber(..)
                                                , RedactedText(..)
                                                , UserID
                                                , UserModel(..)
                                                )
import           Shared.Track.TimeZone          ( stateToTZ )
import           Shared.Transactions.Categorize ( categorize )
import           Shared.Utils                   ( fromRight )

toDbl :: Currency -> Double
toDbl = fromRational . getMonetaryValue

mapExtend :: (a -> c) -> [(a, b)] -> [(a, b, c)]
mapExtend f xys = [ (x, y, f x) | (x, y) <- xys ]

merchantNamer :: Transaction -> Text
merchantNamer Transaction {..} = fromMaybe "Unknown merchant"
  $ asum [cmiName <$> trxMerchant, trxDescription, Nothing]

payeeNamer :: UserModel -> UserID -> UserID -> Text
payeeNamer UserModel {..} thisUser trxUserId =
  if thisUser == trxUserId then "You" else fromMaybe "Your partner" usrFirstName

newtype CurrentUser = CurrentUser UserModel
newtype PayeeUser = PayeeUser UserModel

trxExtract :: CurrentUser -> PayeeUser -> Transaction -> [(Text, Value)]
trxExtract (CurrentUser thisUser) (PayeeUser payee) trx@Transaction {..} =
  [ "merchantName" .= merchantName
    , "transactionAmount" .= dblAmount
    , "usersAmount" .= shareAmount
    , "usersShare" .= toDbl (usersShare * 100)
    , "othersShare" .= toDbl (100 - usersShare * 100)
    , "purchaserName" .= payeeName
    , "purchasedAt" .= trxPurchasedAt
    , "merchant" .= trxMerchant
    , "merchantMcc" .= (cmiMcc <$> trxMerchant)
    , "merchantMccDescription" .= (cmiMccDesc <$> trxMerchant)
    , "merchantLocality" .= (cmiLocality <$> trxMerchant)
    , "merchantRegion" .= (cmiRegion <$> trxMerchant)
    , "merchantCountry" .= (cmiCountry <$> trxMerchant)
    , "transactionCardPresent" .= (pcpIsCardPresent <$> trxDetails)
    , "transactionIsOnline" .= (pcpIsOnline <$> trxDetails)
    , "transactionIsInternational" .= (pcpIsInternational <$> trxDetails)
    , "transactionNetwork" .= (pcpNetwork <$> trxDetails)
    , "transactionIsEMV" .= (pcpIsEMV <$> trxDetails)
    , "transactionType" .= (pcpType <$> trxDetails)
    , "transactionDescription" .= (pcpDescription <$> trxDetails)
    , "transactionContext" .= (pcpContext <$> trxDetails)
    , "transactionId" .= trxId
    , "transactionEventType" .= trxSourceEvent
    , "billingAmount" .= lastBillingAmount
    , "transactionState" .= trxState
    , case trxGroupId of
      Nothing       -> "groupId" .= Null
      Just (gid, _) -> "groupId" .= gid
    , "categories" .= mapExtend categoryDescription (categorize trx)
    , "issuer" .= trxSource
    ]
    <> declineInfo
 where
  merchantName = merchantNamer trx
  payeeName    = payeeNamer payee (usrUserID thisUser) trxUserId
  usersShare =
    trxSplitAmounts
      & filter (\(uid, _) -> uid == usrUserID thisUser)
      & head
      & snd
      & fromRational
      & (/ 100)
  dblAmount = toDbl trxDisplayAmount
  shareAmount :: Double =
    trxDisplayAmount & (*) usersShare & getMonetaryValue & fromRational
  lastBillingAmountMaybe = toDbl . fst <$> listToMaybe trxBillingAmounts
  lastBillingAmount      = fromMaybe dblAmount lastBillingAmountMaybe
  declineInfo            = case trxState of
    TrxDeclined reason -> declineReasonObj reason
    _                  -> []

builder :: Value -> Request -> Request
builder segmentObj req = req
  { method         = renderStdMethod POST
  , requestBody    = RequestBodyLBS $ encode segmentObj
  , requestHeaders = [ ("Content-Type", "application/json")
                     , ("Accept"      , "application/json")
                     , ("User-Agent"  , "Pay Tgthr/1.0")
                     ]
  }

buildReq :: Value -> Either SomeException Request
buildReq segmentObj =
  parseRequest "https://api.segment.io/v1/identify" <&> builder segmentObj

-- inline brittany config for width
-- brittany-next-binding --columns 100
sendUserUpdate :: (Request -> IO a) -> UserModel -> IO ()
sendUserUpdate segmentRequester UserModel {..} = do
  let phoneNumber (PhoneNumber t) = "+1" <> t
  let kycFails = maybe [] getFailureReasons usrAptoKYCStatus
  let segmentObj =
        object
          . filter (\(_, v) -> v /= Null)
          $ [ "userId" .= usrUserID
            , "traits" .= object
              [ "email" .= usrEmail
              , "state" .= show usrUserState
              , "cardState" .= usrAptoCardStatus
              , "kycState" .= case usrAptoKYCStatus of
                Nothing                   -> Null
                Just Passed               -> "Passed"
                Just (AutoVerifyFailed _) -> "AutoVerifyFailed"
                Just (Rejected         _) -> "Rejected"
              , "kycPhoneScoreLow" .= (PhoneScoreLow `elem` kycFails)
              , "kycAddressScoreLow" .= (AddressScoreLow `elem` kycFails)
              , "kycSSNScoreLow" .= (SSNScoreLow `elem` kycFails)
              , "kycNameScoreLow" .= (NameScoreLow `elem` kycFails)
              , "kycWatchlistNeedsReview" .= (WatchlistNeedsReview `elem` kycFails)
              , "kycWatchlistRejected" .= (WatchlistRejected `elem` kycFails)
              , "kycDocumentationIncorrect" .= (DocumentationIncorrect `elem` kycFails)
              , "kycDOBScoreLow" .= (DOBScoreLow `elem` kycFails)
              , "kycIdentityTheftRisk" .= (IdentityTheftRisk `elem` kycFails)
              , "firstName" .= usrFirstName
              , "lastName" .= usrLastName
              , "phone" .= (phoneNumber <$> usrPhone)
              , "birthday" .= usrDOB
              , "address" .= object
                [ "street" .= usrAddressStreet
                , "street2" .= usrAddressStreet2
                , "city" .= usrAddressCity
                , "state" .= usrAddressState
                , "postalCode" .= usrAddressZip
                , ("country", "USA")
                ]
              , "addressStreet" .= usrAddressStreet
              , "addressStreet2" .= usrAddressStreet2
              , "addressCity" .= usrAddressCity
              , "addressState" .= usrAddressState
              , "addressPostalCode" .= usrAddressZip
              , ("addressCountry", "USA")
              , "bankName" .= usrBankName
              , "accountName" .= usrBankAccountName
              , "accountType" .= usrBankType
              , "timezone" .= (stateToTZ <$> usrAddressState)
              , "bankVerified" .= case usrBankVerified of
                Nothing    -> False
                Just False -> False
                Just True  -> True
              , "bankRouting" .= case usrBankRouting of
                Just (RedactedText t) -> String t
                Nothing               -> Null
              , "privacyAccountId" .= fromMaybe "None" usrPrivacyAcctToken
              , "aptoAccountId" .= case usrAptoCardholderID of
                Nothing                   -> "None"
                Just (AptoCardholderId t) -> t
              , "emailVerified" .= usrEmailVerified
              , "phoneVerified" .= usrPhoneVerified
              ]
            , "integrations" .= object ["Slack" .= False]
            ]
      req = buildReq segmentObj
  _ <- segmentRequester $ fromRight req
  return ()

sendUserBalance :: (Request -> IO a) -> UserID -> Currency -> IO ()
sendUserBalance segmentRequester aUser balance = do
  let dblBalance :: Double = fromRational $ getMonetaryValue balance
  trackOneOffTrait segmentRequester
                   aUser
                   ["bankBalance" .= dblBalance, "plaidActive" .= True]

trackOneOffTrait :: (Request -> IO a) -> UserID -> [(Text, Value)] -> IO ()
trackOneOffTrait segmentRequester aUser traits = do
  let segmentObj = object
        [ "userId" .= aUser
        , "traits" .= object traits
        , "integrations" .= object ["Slack" .= False]
        ]
      req = buildReq segmentObj

  _ <- segmentRequester $ fromRight req
  return ()

trackEvent :: (Request -> IO a) -> UserModel -> Text -> Value -> IO ()
trackEvent segmentRequester UserModel {..} event props = do
  let
    sendToAll = case event of
      "" -> False
      _  -> True
    sendToSlack = case event of
      "User KYCDelay" -> True
      "User Active"   -> True
      _               -> False
    segmentObj = object
      [ "userId" .= usrUserID
      , "event" .= event
      , "properties" .= props
      , "integrations" .= object ["All" .= sendToAll, "Slack" .= sendToSlack]
      , "context" .= object
        [ "traits" .= object
            [ "email" .= usrEmail
            , "phone" .= usrPhone
            , "firstName" .= usrFirstName
            , "lastName" .= usrLastName
            , "address" .= object
              [ "city" .= usrAddressCity
              , "state" .= usrAddressState
              , "postalCode" .= usrAddressZip
              , ("country", "USA")
              ]
            ]
        ]
      ]
  let req :: Either SomeException Request =
        parseRequest "https://api.segment.io/v1/track" <&> builder segmentObj
  _ <- segmentRequester $ fromRight req
  return ()

trackGroup :: (Request -> IO a) -> GroupModel -> UserModel -> IO ()
trackGroup segmentRequester group user = do
  let segmentObj = object
        [ "userId" .= usrUserID user
        , "groupId" .= grpId group
        , "integrations" .= object ["Slack" .= True]
        ]
      req :: Either SomeException Request =
        parseRequest "https://api.segment.io/v1/group" <&> builder segmentObj
  _ <- segmentRequester $ fromRight req
  case grpStatus group of
    GroupActive -> trackEvent segmentRequester user "Group Active" Null
    _           -> return ()
  return ()

declineReasonObj :: DeclineReason -> [(Text, Value)]
declineReasonObj theReason =
  [ "declineCase" .= case theReason of
    LowBalance         _                 -> "insufficentFunds"
    BalanceCheckFail   _                 -> "balanceCheckFail"
    ExceedMaxTrxAmount _                 -> "trxAmountLimit"
    InvalidMerchant                      -> "invalidMerchant"
    LostorStolenCard                     -> "lostCard"
    InvalidAmount                        -> "invalidAmount"
    InvalidCardNumber                    -> "invalidCardNumber"
    CardExpired                          -> "cardExpired"
    SuspectFraud                         -> "suspectFraud"
    InternationalNotAllowed              -> "internationalRestricted"
    PinRetriesExceeded                   -> "incorrectPin"
    IncorrectCVV                         -> "incorrectCvv"
    CardNotActivated                     -> "newCard"
    CardInactive                         -> "inactiveCard"
    Shared.Models.Transaction.CardClosed -> "lostCard"
    IncorrectAddress                     -> "incorrectAddress"
    IncorrectPin                         -> "incorrectPin"
    GroupError                           -> "groupError"
    UserNotFound    _                    -> "usernotfound"
    PaymentUnlinked _                    -> "paymentunlinked"
    P2PNotAllowed                        -> "p2pNotAllowed"
    RiskyMerchant                        -> "riskyMerchant"
    UserNotActive _                      -> "usernotactive"
    Unknown       r                      -> r
  , "reason" .= case theReason of
    Unknown _ -> False
    _         -> True
  ]

type PreviousTransactionState = TransactionState
trackTransactionUpdated
  :: (Request -> IO a)
  -> CurrentUser
  -> PayeeUser
  -> PreviousTransactionState
  -> Transaction
  -> IO ()
trackTransactionUpdated segmentRequester currentUser@(CurrentUser thisUser) payee prevState trx
  = trackEvent
    segmentRequester
    thisUser
    "Transaction Updated"
    (object
      (  trxExtract currentUser payee trx
      <> ["previousTransactionState" .= prevState]
      )
    )

createSegmentRequestor :: String -> IO (Request -> IO ())
createSegmentRequestor segmentToken = do
  tlsManager <- newTlsManager
  return $ void . flip httpLbs tlsManager . applyBasicAuth
    (BC.pack segmentToken)
    ""
