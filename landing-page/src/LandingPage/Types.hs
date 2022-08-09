{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards, StrictData #-}

module LandingPage.Types where

import           Control.Concurrent             ( MVar )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.ByteString               as B
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection )
import           GHC.Generics                   ( Generic )
import           LandingPage.Linker.LinkerDB    ( LinkRetriever
                                                , LinkStorer
                                                )
import           Network.HTTP.Client            ( Manager )
import           Network.Wai.Handler.Warp       ( Port )
import           Shared.Amqp                    ( OpaqueChannel )
import qualified Shared.Models.Card            as Card
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.Vault                   ( PlainText )
import qualified Web.ClientSession             as WCS
import           Web.JWT                        ( Signer )

newtype EncryptedSSN = EncryptedSSN Text deriving (Eq, Show)
newtype EncryptedPin = EncryptedPin Text deriving (Eq, Show)
newtype EncryptedToken = EncryptedToken Text deriving (Eq, Show)

data ClusterEnvironment
  = DevelopmentEnv
  | StagingEnv
  | ProductionEnv
  deriving (Eq, Show)

type DBRunner = (Connection -> IO ()) -> IO ()

data AppSettings = AppSettings
  { environment      :: ClusterEnvironment
  , amqpChannel      :: Maybe OpaqueChannel
  , sessionKey       :: WCS.Key
  , port             :: Port
  , ssnEncrypter     :: PlainText -> IO EncryptedSSN
  , pinEncrypter     :: PlainText -> IO EncryptedPin
  , plaidEnvironment :: Text
  , tokenEncrypter   :: PlainText -> IO EncryptedToken
  , tokenDecrypter   :: EncryptedToken -> IO PlainText
  , publicDir        :: String
  , templateDir      :: String
  , linkGet          :: LinkRetriever
  , linkPut          :: LinkStorer
  , domain           :: Text
  , aptoSecrets      :: AptoSecrets
  , withDBPool       :: DBRunner
  , stackDriverInfo  :: Maybe (Manager, (Text, Signer, Text))
  , goingToShutdown  :: MVar Bool
  }

data SessionData = SessionData
  { userID    :: Maybe UserID
  , clientID  :: UUID
  , invitedBy :: Maybe UserID
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data UserSignupStep
  = Login
  | EnterName
  | EnterPartner
  | SetRatio
  | AcceptDisclosures
  | AcceptPolicies
  | EnterPII
  | EnterAddress
  | EnterBanking
  | PlaidLink
  | ChooseChecking
  | ManualBankEntry
  | VerifyBankWaiting
  | VerifyManualBank
  | WaitOnKYC
  | CardShipped
  | Complete
  | Activate
  | CardActive
  | Settings
  | ChangePin
  | ChangeRatio
  | ChangeBank
  | ReviewLegal
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

stepToPath :: UserSignupStep -> Text
stepToPath AcceptDisclosures = "/app/signup/disclosures"
stepToPath EnterName         = "/app/signup/name"
stepToPath EnterPartner      = "/app/signup/partner"
stepToPath EnterPII          = "/app/signup/pii"
stepToPath EnterAddress      = "/app/signup/address"
stepToPath AcceptPolicies    = "/app/signup/legalconsent"
-- Link backend 
stepToPath EnterBanking      = "/app/signup/link"
stepToPath PlaidLink         = "/app/signup/plaid"
stepToPath ChooseChecking    = "/app/signup/checking"
stepToPath ManualBankEntry   = "/app/signup/manuallink/entry"
stepToPath VerifyBankWaiting = "/app/signup/manuallink/waiting"
stepToPath VerifyManualBank  = "/app/signup/manuallink/verify"
-- Link end 
stepToPath Login             = "/app/login"
stepToPath SetRatio          = "/app/signup/ratio"
stepToPath WaitOnKYC         = "/app/signup/kyc"
stepToPath CardShipped       = "/app/signup/kyc"
stepToPath Activate          = "/app/signup/pin"
stepToPath CardActive        = "/app/signup/cardactive"
stepToPath Complete          = "/app/transactions"
stepToPath Settings          = "/app/settings"
stepToPath ChangePin         = "/app/change/pin"
stepToPath ChangeRatio       = "/app/change/ratio"
stepToPath ChangeBank        = "/app/change/bank"
stepToPath ReviewLegal       = "/app/legal"

previousStepForStep :: UserSignupStep -> Maybe UserSignupStep
previousStepForStep step = case step of
  Login             -> Nothing
  EnterName         -> Nothing
  EnterPartner      -> Just EnterName
  SetRatio          -> Just EnterBanking
  AcceptDisclosures -> Just EnterPartner
  EnterPII          -> Just AcceptDisclosures
  EnterAddress      -> Just EnterPII
  AcceptPolicies    -> Just EnterAddress
-- Link backend
  EnterBanking      -> Just AcceptPolicies
  PlaidLink         -> Just EnterBanking
  ChooseChecking    -> Just EnterBanking
  VerifyBankWaiting -> Just EnterBanking
  ManualBankEntry   -> Just EnterBanking
  VerifyManualBank  -> Just EnterBanking
-- 
  WaitOnKYC         -> Just SetRatio
  CardShipped       -> Nothing
  Activate          -> Nothing
  CardActive        -> Nothing
  Complete          -> Nothing
  Settings          -> Just Complete
  ChangePin         -> Just Settings
  ChangeRatio       -> Just Settings
  ChangeBank        -> Just Settings
  ReviewLegal       -> Just Settings

data UserAcceptedGroup
  = GroupAccepted
  | GroupNotYet
  deriving (Eq, Show)

data UserAcceptedRatio
  = RatioAccepted
  | RatioNotYet
  deriving (Eq, Show)

-- inline brittany config for width
-- brittany-next-binding --columns 150
piiChooser :: UserModel -> UserAcceptedGroup -> UserAcceptedRatio -> UserSignupStep
piiChooser model@UserModel {..} userAcceptedGroup userAcceptedRatio
  | isNothing usrFirstName || isNothing usrLastName = EnterName
  | userAcceptedGroup == GroupNotYet = EnterPartner
  | userAcceptedRatio == RatioNotYet = SetRatio
  | isNothing usrDislcosureOk        = AcceptDisclosures
  | isNothing usrConstentOk          = AcceptPolicies
  | isNothing usrPhone || isNothing usrDOB || isNothing usrSSN = EnterPII
  | isNothing usrAddressStreet || isNothing usrAddressCity || isNothing usrAddressState || isNothing usrAddressZip = EnterAddress
  | isNothing usrBankAccountName     = EnterBanking
  | usrBankVerified == Just False    = VerifyManualBank
  | isNothing usrAptoCardId          = cardChooser model
  | isNothing usrAptoCardStatus      = cardChooser model
  | otherwise                        = Complete

cardChooser :: UserModel -> UserSignupStep
cardChooser UserModel {..} = case (usrAptoCardId, usrAptoCardStatus) of
  (Nothing, _                    ) -> WaitOnKYC
  (Just _ , Nothing              ) -> CardShipped
  (Just _ , Just Card.CardCreated) -> CardShipped
  (Just _ , Just _               ) -> Complete

linkChooser :: UserSignupStep -> UserModel -> UserSignupStep
linkChooser otherChooser UserModel {..} = case usrBankVerified of
  Nothing    -> EnterBanking
  Just False -> VerifyBankWaiting
  Just True  -> otherChooser

data PaymentLinked
  = PaymentIsLinked
  | LinkInProgress
  | NoPaymentLinked
  deriving (Eq, Show)

hasLinkedPayment :: UserModel -> PaymentLinked
hasLinkedPayment UserModel {..} =
  case (usrDwollaFundingId, usrBankVerified, usrBankAccountName) of
    (Nothing, Nothing   , Just _) -> PaymentIsLinked -- Plaid    
    (Nothing, _         , _     ) -> NoPaymentLinked
    (Just _ , Just False, _     ) -> LinkInProgress
    (Just _ , Just True , _     ) -> PaymentIsLinked
    (Just _ , Nothing   , _     ) -> PaymentIsLinked -- historical

-- inline brittany config for width
-- brittany-next-binding --columns 100
nextPageChooser :: UserModel -> UserAcceptedGroup -> UserAcceptedRatio -> UserSignupStep
nextPageChooser userModel userAcceptedGroup userAcceptedRatio =
  case (usrUserState userModel, userAcceptedRatio, hasLinkedPayment userModel) of
    (UserCreated        , _            , _              ) -> EnterName
    (UserWaitingOnPII   , _            , _              ) -> piiSwitch
    (UserWaitingOnKYC   , _            , NoPaymentLinked) -> linkChooser piiSwitch userModel
    (UserWaitingOnKYC   , _            , LinkInProgress ) -> linkChooser piiSwitch userModel
    (UserWaitingOnKYC   , _            , PaymentIsLinked) -> WaitOnKYC
    (UserKYCDelay       , _            , NoPaymentLinked) -> linkChooser piiSwitch userModel
    (UserKYCDelay       , _            , LinkInProgress ) -> linkChooser piiSwitch userModel
    (UserKYCDelay       , _            , PaymentIsLinked) -> WaitOnKYC
    (UserActive         , _            , NoPaymentLinked) -> linkChooser piiSwitch userModel
    (UserActive         , _            , LinkInProgress ) -> linkChooser piiSwitch userModel
    (UserActive         , RatioNotYet  , PaymentIsLinked) -> SetRatio
    (UserActive         , RatioAccepted, PaymentIsLinked) -> cardChooser userModel
    (UserUpdated        , _            , _              ) -> piiSwitch
    (UserUpdatedKYCDelay, _            , _              ) -> WaitOnKYC
    (UserClosed _       , _            , _              ) -> Complete
  where piiSwitch = piiChooser userModel userAcceptedGroup userAcceptedRatio


previousPageForStep :: UserSignupStep -> Text
previousPageForStep step = maybe "" stepToPath (previousStepForStep step)

sessionCookieName :: B.ByteString
sessionCookieName = "session"

data AptoSecrets = AptoSecrets
  { aptoBase :: String
  , aptoUser :: B.ByteString
  , aptoPass :: B.ByteString
  }
  deriving Eq
