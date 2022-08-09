{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module APIPrivacy.ExternalAPI.Models where

import           APIPrivacy.Models.Privacy      ( CardSpendLimitDuration
                                                , CardState
                                                , CardType
                                                , Merchant
                                                , TransactionEvent
                                                , TransactionToken
                                                , currencyToCents
                                                , parseTime
                                                , privacyCentsToCurrency
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                , FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value
                                                , object
                                                , withObject
                                                , withText
                                                )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Shared.Models.Card             ( PrivacyCardToken(..) )
import           Shared.Models.Currency         ( Currency )

data TerminalInfo = TerminalInfo
  { attended               :: Bool
  , operator               :: Text
  , onPremise              :: Bool
  , cardRetentionCapable   :: Bool
  , pinCapability          :: Text
  , terminalType           :: Text
  , partialApprovalCapable :: Bool
  }
  deriving (Eq, Show)
instance FromJSON TerminalInfo where
  parseJSON = withObject "TerminalInfo" $ \o -> do
    attended               <- o .: "attended"
    operator               <- o .: "operator"
    onPremise              <- o .: "on_premise"
    cardRetentionCapable   <- o .: "card_a"
    pinCapability          <- o .: "pin_capability"
    terminalType           <- o .: "type"
    partialApprovalCapable <- o .: "partial_approval_capable"

    return TerminalInfo { .. }

data EntryModeInfo = EntryModeInfo
  { pan        :: Text
  , pinEntered :: Bool
  , cardholder :: Text
  , card       :: Text
  }
  deriving (Eq, Show)
instance FromJSON EntryModeInfo where
  parseJSON = withObject "EntryModeInfo" $ \o -> do
    pan        <- o .: "pan"
    pinEntered <- o .: "pin_entered"
    cardholder <- o .: "cardholder"
    card       <- o .: "card"

    return EntryModeInfo { .. }

data PointOfSale = PointOfSale
  { terminal  :: TerminalInfo
  , entryMode :: EntryModeInfo
  }
  deriving (Eq, Show)
instance FromJSON PointOfSale where
  parseJSON = withObject "PointOfSale" $ \o -> do
    terminal  <- o .: "terminal"
    entryMode <- o .: "entry_mode"

    return PointOfSale { .. }

data AVS = AVS
  { zipcode :: Maybe Text
  , address :: Maybe Text
  }
  deriving (Eq, Show)
instance FromJSON AVS where
  parseJSON = withObject "AVS" $ \o -> do
    zipcode <- o .: "zipcode"
    address <- o .: "address"

    return AVS { .. }

data ASAStatus
  = Authorization
  | FinancialAuthorization
  | BalanceInqiry
  deriving (Eq, Show)
instance FromJSON ASAStatus where
  parseJSON = withText "ASAStatus" $ \case
    "AUTHORIZATION"           -> return Authorization
    "FINANCIAL_AUTHORIZATION" -> return FinancialAuthorization
    "BALANCE_INQUIRY"         -> return BalanceInqiry
    t -> fail $ "ASAStatus does not recognize " <> show t

data MiniCard = MiniCard
  { cardToken          :: PrivacyCardToken
  , hostname           :: Text
  , lastFour           :: Text
  , cardState          :: CardState
  , cardType           :: CardType
  , memo               :: Text
  , spendLimit         :: Currency
  , spendLimitDuration :: CardSpendLimitDuration
  }
  deriving (Eq, Show)
instance FromJSON MiniCard where
  parseJSON = withObject "PrivacyCard" $ \o -> do
    hostname           <- o .: "hostname"
    lastFour           <- o .: "last_four"
    memo               <- o .: "memo"
    spendLimit         <- privacyCentsToCurrency <$> o .: "spend_limit"
    spendLimitDuration <- o .: "spend_limit_duration"
    cardState          <- o .: "state"
    cardToken          <- o .: "token"
    cardType           <- o .: "type"

    return MiniCard { .. }

data ASAMessage = ASAMessage
  { amount              :: Currency
  -- | Fee assessed by the merchant and paid for by the cardholder. Will be zero if no fee is assessed. Rebates may be transmitted as a negative value to indicate credited fees.
  , acquirerFee         :: Currency
  -- | The base transaction amount plus the acquirer fee field. This is the amount the issuer should authorize against unless the issuer is paying the acquirer fee on behalf of the cardholder.
  , authorizationAmount :: Currency
  , card                :: MiniCard
  , created             :: UTCTime
  , events              :: [TransactionEvent]
  , funding             :: [Value]
  , merchant            :: Merchant
  -- , pointOfSale         :: PointOfSale
  -- | Contains validation information entered by the client to be verified by the issuer.
  , avs                 :: AVS
  , settledAmount       :: Currency
  -- | AUTHORIZATION, FINANCIAL_AUTHORIZATION and BALANCE_INQUIRY indicates that this request requires an ASA response body in HTTP 200 response. 
  -- | FINANCIAL_AUTHORIZATION is a final single-message transaction with no subsequent clearing.
  , status              :: ASAStatus
  , token               :: TransactionToken
  }
  deriving (Eq, Show)
instance FromJSON ASAMessage where
  parseJSON = withObject "ASAMessage" $ \o -> do
    let centsToCurr = privacyCentsToCurrency
    amount              <- centsToCurr <$> o .: "amount"
    acquirerFee         <- centsToCurr <$> o .: "acquirer_fee"
    authorizationAmount <- centsToCurr <$> o .: "authorization_amount"
    card                <- o .: "card"
    createdTxt          <- o .: "created"
    events              <- o .: "events"
    funding             <- o .: "funding"
    merchant            <- o .: "merchant"
    -- pointOfSale         <- o .: "point_of_sale"
    avs                 <- o .: "avs"
    settledAmount       <- centsToCurr <$> o .: "settled_amount"
    status              <- o .: "status"
    token               <- o .: "token"

    created             <- case parseTime createdTxt of
      Nothing -> fail $ "ASAMessage could not parse created " <> createdTxt
      Just u  -> return u

    return ASAMessage { .. }

data ASAResult
  = Approved
  -- | Card is permanently closed. Using CARD_CLOSED will result in subsequent authorizations being declined on the ASA clients behalf
  -- | Hard Decline
  | CardClosed
  -- | Card is not yet activated, or in a paused state
  | CardPaused
  -- | Same as CARD_PAUSED, will be deprecated in future versions
  | AccountInactive
  -- | Prevent acquirers from approving the transaction despite incorrect AVS. Note: AVS response is not required for this decline type
  | AVSInvalid
  -- | Transaction exceeds issuer set velocity limits. Acquirers may retry the transaction at a later date
  | VelocityExceeded
  -- | Can be used for restricted MCCs, Countries, or transaction types (e.g. Money transfer transactions).
  -- | Hard Decline
  | UnauthorizedMerchant
  -- | User has insufficient funds. Acquirers may retry the transaction at a later time.
  | InsufficentFunds
  -- | A fallback decline for any other decline reason not available in the decline type selection.
  | GenericDecline
  deriving (Eq, Show)
instance ToJSON ASAResult where
  toJSON Approved             = "APPROVED"
  toJSON CardClosed           = "card_a"
  toJSON CardPaused           = "card_a"
  toJSON AccountInactive      = "ACCOUNT_INACTIVE"
  toJSON AVSInvalid           = "AVS_INVALID"
  toJSON VelocityExceeded     = "VELOCITY_EXCEEDED"
  toJSON UnauthorizedMerchant = "UNAUTHORIZED_MERCHANT"
  toJSON InsufficentFunds     = "INSUFFICIENT_FUNDS"
  toJSON GenericDecline       = "GENERIC_DECLINE"

data AVSResult
  = Match
  | Fail
  | MatchAddressOnly
  | MatchZipOnly
  deriving (Eq, Show)
instance ToJSON AVSResult where
  toJSON Match            = "MATCH"
  toJSON Fail             = "Fail"
  toJSON MatchAddressOnly = "MATCH_ADDRESS_ONLY"
  toJSON MatchZipOnly     = "MATCH_ZIP_ONLY"

data BalanceResponse = BalanceResponse
  { amount    :: Currency
  , available :: Currency
  }
  deriving (Eq, Show)
instance ToJSON BalanceResponse where
  toJSON BalanceResponse {..} = object
    [ "amount" .= currencyToCents amount
    , "available" .= currencyToCents available
    ]

data ASAResponse = ASAResponse
  {
  -- | APPROVED to accept the authorization. Any other response will decline the authorization. See ASA Response Result for possible values
    result    :: ASAResult
  -- | The transaction_token from the corresponding ASA Message request
  , token     :: TransactionToken
  -- | ASA Client may return APPROVED with AVS match indicator that can be evaluated by the acquirer. 
  -- | AVS response is optional. If AVS is present and a response is not received, PWP will return AVS validated.
  -- | If AVS attributes aren’t included in the authorization, any AVS response result will be ignored.
  , avsResult :: Maybe AVSResult
  -- | Respective available amount and settled amount values. These values can be used by merchants for authorization decisions as well as balance display at POS/ATM.
  -- | BALANCE_INQUIRY ASA messages require a settled and available amount to be returned. If no balance is returned, PWP will return $0 for both attributes.
  , balance   :: Maybe BalanceResponse
  }
  deriving (Eq, Show)
instance ToJSON ASAResponse where
  toJSON ASAResponse {..} =
    object
      $  ["result" .= result, "token" .= token]
      <> [ "avs_result" .= avsResult | isJust avsResult ]
      <> [ "balance" .= balance | isJust balance ]
