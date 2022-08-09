{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances, OverloadedStrings, StrictData, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use let" -}
{- HLINT ignore "Use lambda-case" -}

module Shared.Models.User
  ( module Shared.Models.User
  , UserID(..)
  , EmailAddress(..)
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID
                                                , nil
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(Escape)
                                                , ToField(..)
                                                , toJSONField
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import qualified Database.PostgreSQL.Simple.Types
                                               as PT
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Base             ( EmailAddress(..)
                                                , UserID(..)
                                                )
import           Shared.Models.Card             ( AptoCardId(..)
                                                , CardStatus
                                                )
import           Shared.Models.KYC              ( KycStatus )
import           Shared.TgthrMessages.Base      ( AccountType(..)
                                                , MessageID(..)
                                                )
import           Shared.Utils                   ( customAesonOptions )
import           Text.Read                      ( readEither )

newtype Password = Password T.Text deriving(Eq, Generic, FromJSON, ToJSON)
instance Show Password where
  show _ = "Password Redacted"
instance ToField Password where
  toField (Password t) = toField t
instance FromField Password where
  fromField a dat = Password <$> fromField a dat

newtype PhoneNumber = PhoneNumber Text deriving (Show, Eq, Generic, FromJSON, ToJSON)
instance ToField PhoneNumber where
  toField (PhoneNumber e) = toField e
instance FromField PhoneNumber where
  fromField f mbs = PhoneNumber <$> fromField f mbs
instance IsString PhoneNumber where
  fromString = PhoneNumber . T.pack
instance ToHttpApiData PhoneNumber where
  toUrlPiece (PhoneNumber t) = t
instance FromHttpApiData PhoneNumber where
  parseUrlPiece t = Right $ PhoneNumber t

{-|
Account Changes a user can make or the system can make
-}
data UserChanges
  -- | Name changed (Required KYC re-check, enforced by `cipCaresAbout`)
  = UsersName
  -- | Address changed (Required KYC re-check, enforced by `cipCaresAbout`)
  | UsersAddress
  -- | Linked bank account info changed
  | UsersBank
  -- | Phone number changed (Required KYC re-check, enforced by `cipCaresAbout`)
  | UsersPhone
  -- | SSN, DOB changed (Required KYC re-check, enforced by `cipCaresAbout`)
  | UsersPII
  -- | Primary Card was changed
  | UsersCard
  -- | KYC Status changed
  | UsersKYC
  -- | Email Address changed
  | UsersEmail
  -- | Consent agreement date changed
  | LegalConsent
  deriving (Eq, Show, Generic)
instance FromJSON UserChanges where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON UserChanges where
  toJSON = genericToJSON customAesonOptions

data FundingInformation = BankFunding
  { accountName :: Maybe Text
  , bankName    :: Maybe Text
  , sourceId    :: Maybe Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON FundingInformation where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON FundingInformation where
  toJSON = genericToJSON customAesonOptions


data ClosureReason
  -- | The user asked for their account to be closed
  = ClosedByUser
  -- | The user did not pass KYC
  | KYCFailed
  -- | The user was fruadulent in their intents
  | FraudyUser
  -- | The user has an overdue balance past the maximum period
  | OverdueBalance
  -- | This user account is a duplicate of another user account
  | DuplicateUser
  -- | Not using a US device to signup
  | ForeignDeviceDuringSignup
  -- | Too long to finish signing up or to make 1st purchase
  | Archived
  -- | Banned Device
  | BannedDeviceId
  -- | Banned IP Address
  | BannedIPAddress
  -- | Banned Device Type
  | BannedDeviceType
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

{-|
= Signup Flows

These flows are the possible ways a user can complete signup

== Green light path customer signup flow 
`UserCreated` -> `UserWaitingOnPII` -> `UserWaitingOnKYC` -> `UserActive` 

1. `UserCreated`: Customer created an account
2. `UserWaitingOnPII`: Customer starts to fill in personal information screen by screen
3. `UserWaitingOnKYC`: Customer finished filling out all required personal information, sent to KYC partner for identitiy verifidaction
4. `UserActive`: Customer passed automatic KYC and can now continue

== Yellow light path for customer signup flow
`UserCreated` -> `UserWaitingOnPII` -> `UserWaitingOnKYC` -> `UserKYCDelay` -> `UserActive`

1. `UserCreated`: Customer created an account
2. `UserWaitingOnPII`: Customer starts to fill in personal information screen by screen
3. `UserWaitingOnKYC`: Customer finished filling out all required personal information, sent to KYC partner for identitiy verifidaction
4. `UserKYCDelay`: Customer failed automatic KYC and must be manually approved
5. `UserActive`: Customer was manually approved and can now continue

== Red Light path for customer signup flow
`UserCreated` -> `UserWaitingOnPII` -> `UserWaitingOnKYC` -> `UserKYCDelay` -> `UserClosed`

1. `UserCreated`: Customer created an account
2. `UserWaitingOnPII`: Customer starts to fill in personal information screen by screen
3. `UserWaitingOnKYC`: Customer finished filling out all required personal information, sent to KYC partner for identitiy verifidaction
4. `UserKYCDelay`: Customer failed automatic KYC and must be manually approved
5. `UserClosed`: Customer was NOT manually approved and their account is closed

== Red Light path for customer signup flow
`UserCreated` -> `UserWaitingOnPII` -> `UserWaitingOnKYC` -> `UserClosed`

1. `UserCreated`: Customer created an account
2. `UserWaitingOnPII`: Customer starts to fill in personal information screen by screen
3. `UserWaitingOnKYC`: Customer finished filling out all required personal information, sent to KYC partner for identitiy verifidaction
4. `UserClosed`: Customer was immediatly rejected and their account was closed

= Active Flows

These flows are only applicable after a user has become active

== User Changes Address
`UserActive` -> `UserUpdated` -> `UserActive`

== User Changes Address
`UserActive` -> `UserUpdated` -> `UserClosed`

== User Changes Address
`UserActive` -> `UserUpdated` -> `UserUpdatedKYCDelay` -> `UserActive`

== User Changes Address
`UserActive` -> `UserUpdated` -> `UserUpdatedKYCDelay` -> `UserClosed`

== User Closes Account
`UserActive` -> `UserClosed`
-}
data UserState
  -- | User has been created, no PII has yet to be collected
  = UserCreated
  -- | Some user PII has been entered
  | UserWaitingOnPII
  -- | All uer PII has been entered, running KYC on user
  | UserWaitingOnKYC
  -- | Automatic KYC did not pass, user is waiting on manual review
  | UserKYCDelay
  -- | User is active and can get cards, make purchases, etc
  | UserActive
  -- | User was once `UserActive` but has updated their information, needs KYC again
  | UserUpdated
  -- | User was once `UserActive` but did not pass automatic KYC, manual review required
  | UserUpdatedKYCDelay
  -- | User's account has been closed for the included reason
  | UserClosed ClosureReason
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)
instance ToField UserState where
  toField UserCreated         = Escape "created"
  toField UserWaitingOnPII    = Escape "waitingOnPII"
  toField UserWaitingOnKYC    = Escape "waitingOnKYC"
  toField UserKYCDelay        = Escape "initial_kyc_delay"
  toField UserUpdated         = Escape "updated"
  toField UserUpdatedKYCDelay = Escape "updated_kyc_delay"
  toField UserActive          = Escape "active"
  toField (UserClosed _)      = Escape "closed"
instance FromHttpApiData UserState where
  parseUrlPiece t = case readEither (T.unpack t) of
    Right s -> Right s
    Left  s -> Left $ T.pack s
instance ToHttpApiData UserState where
  toUrlPiece = T.pack . show

fromUserID :: UserID -> UUID
fromUserID (UserID u) = u

newtype RedactedText = RedactedText T.Text deriving (Eq, Generic, ToJSON, FromJSON)
instance Show RedactedText where
  show _ = "**Redacted**"
instance ToField RedactedText where
  toField (RedactedText t) = toField t
instance FromField RedactedText where
  fromField a dat = RedactedText <$> fromField a dat
instance IsString RedactedText  where
  fromString = RedactedText . Data.String.fromString

fromRedacted :: RedactedText -> Text
fromRedacted (RedactedText t) = t

instance ToField AptoCardholderId where
  toField (AptoCardholderId t) = toField t
instance FromField AptoCardholderId where
  fromField a dat = AptoCardholderId <$> fromField a dat

instance ToField AptoCardId where
  toField (AptoCardId t) = toField t
instance FromField AptoCardId where
  fromField a dat = AptoCardId <$> fromField a dat

data UserModel = UserModel
  { usrUserID             :: UserID
  -- ^ The user's unique Id
  , usrUserState          :: UserState
  -- ^ The user's current state
  , usrRevision           :: Integer
  -- ^ monotonically increasing integer to defend against stale updated
  , usrVersion            :: T.Text
  -- ^ User Model version
  , usrEmail              :: EmailAddress
  -- ^ User's email address
  , usrMsgSource          :: MessageID
  -- User controlled below
  , usrPassword           :: Maybe Password
  -- Name
  , usrFirstName          :: Maybe T.Text
  , usrLastName           :: Maybe T.Text
  -- Address
  , usrAddressStreet      :: Maybe T.Text
  , usrAddressStreet2     :: Maybe T.Text
  , usrAddressCity        :: Maybe T.Text
  , usrAddressState       :: Maybe T.Text
  , usrAddressZip         :: Maybe T.Text
  -- ACH Info
  , usrBankRouting        :: Maybe RedactedText
  , usrBankAcount         :: Maybe RedactedText
  , usrBankAccountName    :: Maybe T.Text
  , usrBankName           :: Maybe T.Text
  , usrBankType           :: Maybe AccountType
  , usrBankVerified       :: Maybe Bool
  , usrBankVerifedAmounts :: Maybe [Double] -- (2)
  , usrDwollaId           :: Maybe T.Text
  , usrDwollaFundingId    :: Maybe T.Text
  -- PII
  , usrPhone              :: Maybe PhoneNumber
  , usrDOB                :: Maybe UTCTime
  , usrSSN                :: Maybe RedactedText
  -- User Signup State
  , usrDislcosureOk       :: Maybe UTCTime
  , usrConstentOk         :: Maybe UTCTime
  -- Apto
  , usrAptoCardholderID   :: Maybe AptoCardholderId
  , usrAptoKYCStatus      :: Maybe KycStatus
  , usrAptoCardId         :: Maybe AptoCardId -- CardModel.cardPlatform
  , usrAptoCardStatus     :: Maybe CardStatus -- CardModel.cardStatus
  -- Dates
  , usrCreatedOn          :: UTCTime
  , usrFirstSignIn        :: Maybe UTCTime
  , usrBecameActiveOn     :: Maybe UTCTime
  , usrCardCreatedOn      :: Maybe UTCTime
  , usrCardActivatedOn    :: Maybe UTCTime
  -- Privacy
  , usrPrivacyAcctToken   :: Maybe Text
  -- Verification
  , usrEmailVerified      :: Bool
  -- ^ Has the user's email been verified
  , usrPhoneVerified      :: Bool
  -- ^ Has the user's phone been verified
  }
  deriving (Show, Eq, Generic)
instance FromJSON UserModel where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON UserModel where
  toJSON = genericToJSON customAesonOptions

instance ToField [Double] where
  toField = toJSONField
instance FromField [Double] where
  fromField = fromJSONField

instance ToRow UserModel where
  toRow UserModel {..} =
    [ toField usrUserID
    , toField usrRevision
    , toField usrVersion
    , toField usrMsgSource
    , toField usrEmail
    , toField usrPassword
    , toField usrFirstName
    , toField usrLastName
    , toField usrAddressStreet
    , toField usrAddressStreet2
    , toField usrAddressCity
    , toField usrAddressState
    , toField usrAddressZip
    , toField usrBankRouting
    , toField usrBankAcount
    , toField usrPhone
    , toField usrDOB
    , toField usrSSN
    , toField usrDislcosureOk
    , toField usrConstentOk
    , toField usrAptoCardholderID
    , toField usrAptoKYCStatus
    , toField usrAptoCardId
    , toField usrAptoCardStatus
    , toField usrUserState
    , toField $ case usrUserState of
      UserClosed ClosedByUser              -> Just "closedByUser"
      UserClosed KYCFailed                 -> Just "kycFailed"
      UserClosed FraudyUser                -> Just "fraudyUser"
      UserClosed OverdueBalance            -> Just "overdueBalance"
      UserClosed DuplicateUser             -> Just "duplicateUser"
      UserClosed ForeignDeviceDuringSignup -> Just "ForeignDeviceDuringSignup"
      UserClosed Archived                  -> Just "Archived"
      UserClosed BannedDeviceId            -> Just "BannedDeviceId"
      UserClosed BannedIPAddress           -> Just "BannedIPAddress"
      UserClosed BannedDeviceType          -> Just "BannedDeviceType"
      UserCreated                          -> Nothing :: Maybe Text
      UserWaitingOnPII                     -> Nothing :: Maybe Text
      UserActive                           -> Nothing :: Maybe Text
      UserWaitingOnKYC                     -> Nothing :: Maybe Text
      UserKYCDelay                         -> Nothing :: Maybe Text
      UserUpdated                          -> Nothing :: Maybe Text
      UserUpdatedKYCDelay                  -> Nothing :: Maybe Text
    , toField usrBankAccountName
    , toField usrBankName
    , toField usrDwollaId
    , toField usrDwollaFundingId
    , toField usrBankType
    , toField usrBankVerified
    , toField usrBankVerifedAmounts
    , toField usrCreatedOn
    , toField usrFirstSignIn
    , toField usrBecameActiveOn
    , toField usrCardCreatedOn
    , toField usrCardActivatedOn
    , toField usrPrivacyAcctToken
    , toField usrEmailVerified
    , toField usrPhoneVerified
    ]
instance FromRow UserModel where
  fromRow = do
    usrUserID             <- field
    usrRevision           <- field
    usrVersion            <- field
    usrMsgSource          <- field
    usrEmail              <- field
    usrPassword           <- field
    usrFirstName          <- field
    usrLastName           <- field
    usrAddressStreet      <- field
    usrAddressStreet2     <- field
    usrAddressCity        <- field
    usrAddressState       <- field
    usrAddressZip         <- field
    usrBankRouting        <- field
    usrBankAcount         <- field
    usrPhone              <- field
    usrDOB                <- field
    usrSSN                <- field
    usrDislcosureOk       <- field
    usrConstentOk         <- field
    usrAptoCardholderID   <- field
    usrAptoKYCStatus      <- field
    usrAptoCardId         <- field
    usrAptoCardStatus     <- field
    status :: String      <- field
    closure :: Maybe Text <- field
    usrBankAccountName    <- field
    usrBankName           <- field
    usrDwollaId           <- field
    usrDwollaFundingId    <- field
    usrBankType           <- field
    usrBankVerified       <- field
    usrBankVerifedAmounts <- field
    usrCreatedOn          <- field
    usrFirstSignIn        <- field
    usrBecameActiveOn     <- field
    usrCardCreatedOn      <- field
    usrCardActivatedOn    <- field
    usrPrivacyAcctToken   <- field
    usrEmailVerified      <- field
    usrPhoneVerified      <- field

    usrUserState          <- return $ case (status, closure) of
      ("created"          , _                      ) -> UserCreated
      ("waitingOnPII"     , _                      ) -> UserWaitingOnPII
      ("waitingOnApto"    , _                      ) -> UserWaitingOnKYC --historical
      ("waitingOnDwolla"  , _                      ) -> UserWaitingOnKYC --historical
      ("waitingOnKYC"     , _                      ) -> UserWaitingOnKYC
      ("initial_kyc_delay", _                      ) -> UserKYCDelay
      ("updated"          , _                      ) -> UserUpdated
      ("updated_kyc_delay", _                      ) -> UserUpdatedKYCDelay
      ("active"           , _                      ) -> UserActive
      ("closed"           , Just "closedByUser"    ) -> UserClosed ClosedByUser
      ("closed"           , Just "kycFailed"       ) -> UserClosed KYCFailed
      ("closed"           , Just "fraudyUser"      ) -> UserClosed FraudyUser
      ("closed", Just "overdueBalance") -> UserClosed OverdueBalance
      ("closed"           , Just "duplicateUser"   ) -> UserClosed DuplicateUser
      ("closed"           , Just "Archived"        ) -> UserClosed Archived
      ("closed", Just "BannedDeviceId") -> UserClosed BannedDeviceId
      ("closed", Just "BannedIPAddress") -> UserClosed BannedIPAddress
      ("closed", Just "BannedDeviceType") -> UserClosed BannedDeviceType
      ("closed", Just "ForeignDeviceDuringSignup") ->
        UserClosed ForeignDeviceDuringSignup
      ("closed", _) -> error "Closed must have a known reason"
      (v       , _) -> error $ "Unknown UserModel status \"" <> v <> "\""

    return UserModel { .. }

data UserTrait
  = NameFirst
  | NameLast
  | AddressStreet
  | AddressStreet2
  | AddressCity
  | AddressState
  | AddressZip
  | FsACHBankRoutingNum
  | FsACHBankAccountNum
  | FsACHBankName
  | FsACHBankAccountName
  | FsACHBankAccountType
  | DateOfBirth
  | EncryptedSSN
  | Phone
  | PrivacyAccountToken
  | DwollaCustomerId
  | DwollaFundingId
  deriving (Eq, Show, Generic)
instance ToJSON UserTrait
instance FromJSON UserTrait

userModelFieldOrder :: PT.Query
userModelFieldOrder =
  " id, revision, version, msg_source, email, password, name_first, name_last, address_street, address_street2, address_city, address_state, address_zip, bank_routing, bank_account, phone_number, dob, ssn, disclosure_accept, consent_accept, apto_cardholderid, apto_kyc_status, apto_cardid, apto_cardstatus, status, closure_reason, bank_nickname, bank_name, dwolla_customerid, dwolla_fundingid, bank_type, bank_verified, bank_verified_amounts, created_on, first_signin_on, activated_on, card_created_on, card_activated_on, privacy_cardholderid, email_verified, phone_verified "

userModelPlaceHolders :: PT.Query
userModelPlaceHolders =
  " ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? "

defaultUser :: UTCTime -> EmailAddress -> UserModel
defaultUser now anEmail = UserModel { usrVersion            = "1.0"
                                    , usrRevision           = 1
                                    , usrFirstName          = Nothing
                                    , usrLastName           = Nothing
                                    , usrUserState          = UserCreated
                                    , usrUserID             = UserID nil
                                    , usrMsgSource          = MessageID nil
                                    , usrEmail              = anEmail
                                    , usrPassword           = Nothing
                                    , usrAddressStreet      = Nothing
                                    , usrAddressStreet2     = Nothing
                                    , usrAddressCity        = Nothing
                                    , usrAddressState       = Nothing
                                    , usrAddressZip         = Nothing
                                    , usrBankRouting        = Nothing
                                    , usrBankAcount         = Nothing
                                    , usrBankType           = Nothing
                                    , usrPhone              = Nothing
                                    , usrDOB                = Nothing
                                    , usrSSN                = Nothing
                                    , usrDislcosureOk       = Nothing
                                    , usrConstentOk         = Nothing
                                    , usrAptoCardholderID   = Nothing
                                    , usrAptoKYCStatus      = Nothing
                                    , usrAptoCardId         = Nothing
                                    , usrAptoCardStatus     = Nothing
                                    , usrBankAccountName    = Nothing
                                    , usrBankName           = Nothing
                                    , usrDwollaId           = Nothing
                                    , usrDwollaFundingId    = Nothing
                                    , usrBankVerified       = Nothing
                                    , usrBankVerifedAmounts = Nothing
                                    , usrCreatedOn          = now
                                    , usrFirstSignIn        = Nothing
                                    , usrBecameActiveOn     = Nothing
                                    , usrCardCreatedOn      = Nothing
                                    , usrCardActivatedOn    = Nothing
                                    , usrPrivacyAcctToken   = Nothing
                                    , usrEmailVerified      = False
                                    , usrPhoneVerified      = False
                                    }

-- | Normalized Email to lowercase and no whitespace around
-- 
-- >>> normalizeEmail $ EmailAddress "ADAM@example.com"
-- EmailAddress "adam@example.com"
-- 
-- >>> normalizeEmail $ EmailAddress "   ADAM@example.com    "
-- EmailAddress "adam@example.com"
normalizeEmail :: EmailAddress -> EmailAddress
normalizeEmail (EmailAddress e) = EmailAddress . T.toLower . T.strip $ e

changesRequireReKYC :: Foldable t => UserState -> t UserChanges -> Bool
changesRequireReKYC currentState changes =
  let cipCares = foldr (\ch acc -> acc || cipCaresAbout ch) False changes
  in  case (cipCares, currentState) of
        (False, _                  ) -> False
        (True , UserCreated        ) -> False
        (True , UserWaitingOnPII   ) -> False
        (True , UserWaitingOnKYC   ) -> False
        (True , UserKYCDelay       ) -> True
        (True , UserActive         ) -> True
        (True , UserUpdated        ) -> False
        (True , UserUpdatedKYCDelay) -> True
        (True , UserClosed _       ) -> False

-- | Does our KYC process care about this change?
cipCaresAbout :: UserChanges -> Bool
cipCaresAbout UsersName    = True
cipCaresAbout UsersAddress = True
cipCaresAbout UsersBank    = False
cipCaresAbout UsersPhone   = True
cipCaresAbout UsersPII     = True
cipCaresAbout UsersCard    = False
cipCaresAbout UsersKYC     = False
cipCaresAbout UsersEmail   = True
cipCaresAbout LegalConsent = False
