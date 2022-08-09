{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE StrictData #-}

module APIDwolla.Dwolla.Types where

import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock

data MicrodepositTopic
  = MDAdded
  | MDFailed
  | MDCompleted
  | MDMaxAttempts
  deriving (Eq, Show)

data FundingSourceTopic
  = SourceAdded
  | SourceRemoved
  | SourceVerified
  | SourceUnverified
  | SourceNegative
  | SourceUpdated
  | MicroDeposit MicrodepositTopic
  deriving (Eq, Show)

data CustomerVeficationTopic
  = DocumentNeeded
  | DocumentUploaded
  | DocumentFailed
  | DocumentApproved
  | ReverificationNeeded
  deriving (Eq, Show)

data CustomerAccountTopic
  = CCreated
  | Verification CustomerVeficationTopic
  | Verified
  | Suspended
  | Activated
  | Deactivated
  deriving (Eq, Show)

data TransferTopic
  = TCreated
  | CreationFailed
  | Cancelled
  | Failed
  | Completed
  deriving (Eq, Show)

data MassPaymentTopic
  = MPCreated
  | MPCompleted
  | MPCancelled
  | MPBalanceInquiryCompleted
  deriving (Eq, Show)

data CustomerTopic
  = Account CustomerAccountTopic
  | Funding FundingSourceTopic
  | TransferBank TransferTopic
  | Transfer TransferTopic
  | MassPayment MassPaymentTopic
  deriving (Eq, Show)

data DwollaEventTopic
  = Unknown Text
  | MasterAccountActivated
  | MasterAccountSuspended
  | StatementCreated
  | MasterAccount CustomerTopic
  | Customer CustomerTopic
  deriving (Eq, Show)

instance FromJSON DwollaEventTopic where
  parseJSON = withText "DwollaEventTopic" $ \t -> return $ case t of
-- #  Dwolla Master Account Event topics
-- ## Accounts
    "account_suspended"         -> MasterAccountActivated
    "account_activated"         -> MasterAccountSuspended
-- ## Funding Sources
    "funding_source_added"      -> MasterAccount (Funding SourceAdded)
    "funding_source_removed"    -> MasterAccount (Funding SourceRemoved)
    "funding_source_verified"   -> MasterAccount (Funding SourceVerified)
    "funding_source_unverified" -> MasterAccount (Funding SourceUnverified)
    "funding_source_negative"   -> MasterAccount (Funding SourceNegative)
    "funding_source_updated"    -> MasterAccount (Funding SourceUpdated)
    "microdeposits_added" -> MasterAccount (Funding (MicroDeposit MDAdded))
    "microdeposits_failed" -> MasterAccount (Funding (MicroDeposit MDFailed))
    "microdeposits_completed" ->
      MasterAccount (Funding (MicroDeposit MDCompleted))
    "microdeposits_maxattempts" ->
      MasterAccount (Funding (MicroDeposit MDMaxAttempts))
-- ## Transfers
    "bank_transfer_created"   -> MasterAccount (TransferBank TCreated)
    "bank_transfer_cancelled" -> MasterAccount (TransferBank Cancelled)
    "bank_transfer_failed"    -> MasterAccount (TransferBank Failed)
    "bank_transfer_completed" -> MasterAccount (TransferBank Completed)
    "transfer_created"        -> MasterAccount (Transfer TCreated)
    "transfer_cancelled"      -> MasterAccount (Transfer Cancelled)
    "transfer_failed"         -> MasterAccount (Transfer Failed)
    "transfer_completed"      -> MasterAccount (Transfer Completed)
-- ## Mass Payments
    "mass_payment_created"    -> MasterAccount (MassPayment MPCreated)
    "mass_payment_completed"  -> MasterAccount (MassPayment MPCompleted)
    "mass_payment_cancelled"  -> MasterAccount (MassPayment MPCancelled)
-- ## Statements
    "statement_created"       -> StatementCreated
-- #  Customer Account Event topics
-- ## Customers
    "customer_created"        -> Customer (Account CCreated)
    "customer_verification_document_needed" ->
      Customer (Account (Verification DocumentNeeded))
    "customer_verification_document_uploaded" ->
      Customer (Account (Verification DocumentUploaded))
    "customer_verification_document_failed" ->
      Customer (Account (Verification DocumentFailed))
    "customer_verification_document_approved" ->
      Customer (Account (Verification DocumentApproved))
    "customer_reverification_needed" ->
      Customer (Account (Verification ReverificationNeeded))
    "customer_verified"                  -> Customer (Account Verified)
    "customer_suspended"                 -> Customer (Account Suspended)
    "customer_activated"                 -> Customer (Account Activated)
    "customer_deactivated"               -> Customer (Account Deactivated)
-- ## Funding Sources
    "customer_funding_source_added"      -> Customer (Funding SourceAdded)
    "customer_funding_source_removed"    -> Customer (Funding SourceRemoved)
    "customer_funding_source_verified"   -> Customer (Funding SourceVerified)
    "customer_funding_source_unverified" -> Customer (Funding SourceUnverified)
    "customer_funding_source_negative"   -> Customer (Funding SourceNegative)
    "customer_funding_source_updated"    -> Customer (Funding SourceUpdated)
    "customer_microdeposits_added" -> Customer (Funding (MicroDeposit MDAdded))
    "customer_microdeposits_failed" ->
      Customer (Funding (MicroDeposit MDFailed))
    "customer_microdeposits_completed" ->
      Customer (Funding (MicroDeposit MDCompleted))
    "customer_microdeposits_maxattempts" ->
      Customer (Funding (MicroDeposit MDMaxAttempts))
-- ## Transfers
    "customer_bank_transfer_created" -> Customer (TransferBank TCreated)
    "customer_bank_transfer_creation_failed" ->
      Customer (TransferBank CreationFailed)
    "customer_bank_transfer_cancelled" -> Customer (TransferBank Cancelled)
    "customer_bank_transfer_failed"    -> Customer (TransferBank Failed)
    "customer_bank_transfer_completed" -> Customer (TransferBank Completed)
    "customer_transfer_created"        -> Customer (Transfer TCreated)
    "customer_transfer_cancelled"      -> Customer (Transfer Cancelled)
    "customer_transfer_failed"         -> Customer (Transfer Failed)
    "customer_transfer_completed"      -> Customer (Transfer Completed)
-- ## Mass Payments
    "customer_mass_payment_created"    -> Customer (MassPayment MPCreated)
    "customer_mass_payment_completed"  -> Customer (MassPayment MPCompleted)
    "customer_mass_payment_cancelled"  -> Customer (MassPayment MPCancelled)
    "customer_balance_inquiry_completed" ->
      Customer (MassPayment MPBalanceInquiryCompleted)
-- # OTHER
    _ -> Unknown t

data DwollaEvent = DwollaEvent
  { linkSelf :: Text
  , linkResource :: Text
  , linkAccount :: Text
  , linkCustomer :: Maybe Text
  , eventId :: Text
  , eventCreatedAt :: UTCTime
  , eventTopic ::DwollaEventTopic
  , resourceId :: Text
  } deriving (Eq, Show)
instance FromJSON DwollaEvent where
  parseJSON = withObject "DwollaEvent" $ \o -> do
    links        <- o .: "_links"
    linkSelf     <- links .: "self" >>= (.: "href")
    linkResource <- links .: "resource" >>= (.: "href")
    linkAccount  <- links .: "account" >>= (.: "href")
    linkCustomer <-
      links
      .:? "customer"
      >>= (\a -> case a of
            Just obj -> obj .: "href"
            Nothing  -> return Nothing
          )
    eventId        <- o .: "id"
    eventCreatedAt <- o .: "created"
    eventTopic     <- o .: "topic"
    resourceId     <- o .: "resourceId"
    return DwollaEvent { .. }

data DwollaLink = DwollaLink
  { linkHref :: Text
  , linkType :: Text
  , linkResourceType :: Text
  } deriving (Eq, Show)
instance FromJSON DwollaLink where
  parseJSON = withObject "DwollaLink" $ \o -> do
    linkHref         <- o .: "href"
    linkType         <- o .: "type"
    linkResourceType <- o .: "resource-type"
    return DwollaLink { .. }

data DwollaErrorReason
  = DwollaValidationError [DwollaEmbeddedError]
  | DwollaDuplicateResource DwollaLink
   deriving (Eq, Show)

data DwollaError = DwollaError
  { errorCode :: DwollaErrorReason
  , errorMessage :: Text
  } deriving (Eq, Show)
instance FromJSON DwollaError where
  parseJSON = withObject "DwollaError" $ \o -> do
    errorCodeText :: Text <- o .: "code"
    errorMessage          <- o .: "message"
    case errorCodeText of
      "ValidationError" -> do
        errorEmbed <- o .: "_embedded" >>= (.: "errors")
        let errorCode = DwollaValidationError errorEmbed
        return DwollaError { .. }
      "DuplicateResource" -> do
        errorLinks <- o .: "_links" >>= (.: "about")
        let errorCode = DwollaDuplicateResource errorLinks
        return DwollaError { .. }
      t -> fail $ "Unknown errorCodeText: \"" <> T.unpack t <> "\""

data DwollaEmbeddedError = DwollaEmbeddedError
  { validationError :: DwollaValidationErrorCode
  , validationMessage :: Text
  , validationPath :: Text
  }
  deriving (Eq, Show)
instance FromJSON DwollaEmbeddedError where
  parseJSON = withObject "DwollaEmbeddedError" $ \o -> do
    validationError   <- o .: "code"
    validationMessage <- o .: "message"
    validationPath    <- o .: "path"
    return DwollaEmbeddedError { .. }

data DwollaValidationErrorCode
  = InvalidFormat
  | Restricted
  | InvalidFundingSource
  deriving (Eq, Show)
instance FromJSON DwollaValidationErrorCode where
  parseJSON = withText "DwollaValidationErrorCode" $ \t -> case t of
    "Restricted"    -> return Restricted
    "Invalid"       -> return InvalidFundingSource
    "InvalidFormat" -> return InvalidFormat
    _ -> fail $ "Unknown DwollaErrorCode: \"" <> T.unpack t <> "\""
