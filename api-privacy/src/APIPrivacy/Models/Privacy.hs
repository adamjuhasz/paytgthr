{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module APIPrivacy.Models.Privacy
  ( module APIPrivacy.Models.Privacy
  , Currency(..)
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (<?>)
                                                , FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , withObject
                                                , withText
                                                )
import           Data.Aeson.Types               ( JSONPathElement(Key) )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , parseTimeM
                                                )
import           Servant                        ( ToHttpApiData(toUrlPiece) )
import           Shared.Models.Card             ( PrivacyCardToken(..) )
import           Shared.Models.Cardholder       ( PrivacyAccountToken(..) )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.User             ( RedactedText(..) )
import           Text.Read                      ( readEither )

currencyToCents :: Currency -> Int
currencyToCents (Currency _ c) = ceiling (fromRational (c * 100) :: Double)

parseTime :: String -> Maybe UTCTime
parseTime t =
  parseTimeM True defaultTimeLocale "%FT%T%QZ" t
    <|> parseTimeM True defaultTimeLocale "%FT%T%Q%z" t
    <|> parseTimeM True defaultTimeLocale "%F %T%Q"   t -- Bug workaround For `2021-04-21 21:52:48`

data FundingAccountState
  -- | The funding account is available to use for card creation and transactions
  = AccountEnabled
  -- | The funding account is still being verified e.g. bank micro-deposits verification
  | AccountPending
  deriving (Eq, Show)
instance FromJSON FundingAccountState where
  parseJSON = withText "FundingAccountState" $ \case
    "ENABLED" -> return AccountEnabled
    "PENDING" -> return AccountPending
    t         -> fail $ "FundingAccountState doesn't recognize " <> show t

data FundingAccountType
  -- | Bank checking account
  = AccountDepositoryChecking
  -- | Bank savings account
  | AccountDepositorySavings
  -- | Debit card
  | AccountDebitCard
  deriving (Eq, Show)
instance FromJSON FundingAccountType where
  parseJSON = withText "FundingAccountType" $ \case
    "DEPOSITORY_CHECKING" -> return AccountDepositoryChecking
    "DEPOSITORY_SAVINGS"  -> return AccountDepositorySavings
    "card_a"          -> return AccountDebitCard
    t -> fail $ "FundingAccountType doesn't recognize " <> show t

newtype FundingAccountToken = FundingAccountToken Text deriving (Eq, Show)
instance FromJSON FundingAccountToken where
  parseJSON = withText "FundingAccountToken" $ return . FundingAccountToken
instance ToJSON FundingAccountToken where
  toJSON (FundingAccountToken t) = toJSON t
instance ToHttpApiData FundingAccountToken where
  toUrlPiece (FundingAccountToken t) = t

data FundingAccount = FundingAccount
  { accountName  :: Maybe Text
  , created      :: Maybe UTCTime
  , lastFour     :: Maybe Text
  , nickname     :: Maybe Text
  , accountState :: FundingAccountState
  , accountToken :: FundingAccountToken
  , accountType  :: FundingAccountType
  }
  deriving (Eq, Show)
instance FromJSON FundingAccount where
  parseJSON = withObject "FundingAccount" $ \o -> do
    accountName  <- o .: "account_name"
    createdTxt   <- o .: "created"
    lastFour     <- o .: "last_four"
    nickname     <- o .: "nickname"
    accountState <- o .: "state"
    accountToken <- o .: "token"
    accountType  <- o .: "type"

    created      <- case (createdTxt, createdTxt >>= parseTime) of
      (Nothing, _) -> return Nothing
      (Just txt, Nothing) ->
        fail $ "FundingAccount could not parse created " <> txt
      (Just _, Just u) -> return $ Just u

    return FundingAccount { .. }

data CardExpiration = CardExpiration
  { expMonth :: Int
  , expYear  :: Int
  }
  deriving (Eq, Show)

data CardSpendLimitDuration
  -- | Card will authorizate multiple transactions if each individual transaction is under the spend limit
  = TransactionLimit
  -- | Card will authorize transactions up to spend limit for the trailing month. (Note month is calculated as this calendar date one month prior)
  | MonthlyLimit
  -- | Card will authorize transactions up to spend limit in a calendar year
  | AnnualLimit
  -- | Card will authorize only up to spend limit for the entire lifetime of the card
  | Forever
  deriving (Eq, Show)
instance FromJSON CardSpendLimitDuration where
  parseJSON = withText "CardSpendLimitDuration" $ \case
    "TRANSACTION" -> return TransactionLimit
    "MONTHLY"     -> return MonthlyLimit
    "ANNUALLY"    -> return AnnualLimit
    "FOREVER"     -> return Forever
    t -> fail $ "CardSpendLimitDuration doesn't recognize " <> show t

data CardState
  -- | Card will approve authorizations (if they match card and account parameters)
  = CardOpen
  -- | Card will decline authorizations, but can be resumed at a later time
  | CardPaused
  -- | Card will no longer approve authorizations. Closing a card cannot be undone
  | CardClosed
  -- | The initial state for cards of type PHYSICAL. The card is provisioned pending manufacturing and fulfillment. Cards in this state can accept authorizations for e-commerce purchases, but not for "Card Present" purchases where the physical card itself is present.
  | CardPendingFulfillment
  -- | Each business day at 2pm Eastern Time Zone (ET), cards of type PHYSICAL in state PENDING_FULFILLMENT are sent to the card production warehouse and updated to state PENDING_ACTIVATION. Similar to PENDING_FULFILLMENT, cards in this state can be used for e-commerce transactions. API clients should update the card's state to OPEN only after the cardholder confirms receipt of the card.
  | CardPendingActivation
  deriving (Eq, Show)
instance FromJSON CardState where
  parseJSON = withText "CardState" $ \case
    "OPEN"                -> return CardOpen
    "PAUSED"              -> return CardPaused
    "CLOSED"              -> return CardClosed
    "PENDING_FULFILLMENT" -> return CardPendingFulfillment
    "PENDING_ACTIVATION"  -> return CardPendingActivation
    t                     -> fail $ "CardState doesn't recognize " <> show t
instance ToJSON CardState where
  toJSON CardOpen               = "OPEN"
  toJSON CardPaused             = "PAUSED"
  toJSON CardClosed             = "CLOSED"
  toJSON CardPendingFulfillment = "PENDING_FULFILLMENT"
  toJSON CardPendingActivation  = "PENDING_ACTIVATION"

data CardType
  -- | Card will close shortly after the first transaction
  = SingleUseCard
  -- | Card is locked to first merchant that successfully authorizes the card
  | MerchantLockedCard
  -- | Card will authorize at any merchant. Creating these cards requires additional privileges
  | UnlockedCard
  -- | Manufactured and sent to the cardholder. We offer white label branding, credit, ATM, PIN debit, chip/EMV, NFC and magstripe functionality
  | PhysiscalCard
  -- | Cards that can be provisioned to a digital wallet like Google Pay or Apple Wallet.
  | DigitalWalletCard
  deriving (Eq, Show)
instance FromJSON CardType where
  parseJSON = withText "CardType" $ \case
    "SINGLE_USE"      -> return SingleUseCard
    "MERCHANT_LOCKED" -> return MerchantLockedCard
    "UNLOCKED"        -> return UnlockedCard
    "PHYSICAL"        -> return PhysiscalCard
    "DIGITAL_WALLET"  -> return DigitalWalletCard
    t                 -> fail $ "CardType doesn't recognize " <> show t
instance ToJSON CardType where
  toJSON SingleUseCard      = "SINGLE_USE"
  toJSON MerchantLockedCard = "MERCHANT_LOCKED"
  toJSON UnlockedCard       = "UNLOCKED"
  toJSON PhysiscalCard      = "PHYSICAL"
  toJSON DigitalWalletCard  = "DIGITAL_WALLET"

data PrivacyCard = PrivacyCard
  { created            :: UTCTime
  , pciCVV             :: Maybe Text -- PCI
  -- , fundingAccount     :: Maybe FundingAccount
  , pciExpiration      :: Maybe CardExpiration
  , hostname           :: Maybe Text
  , lastFour           :: Text
  , memo               :: Text
  , pciPAN             :: Maybe RedactedText
  , spendLimit         :: Currency
  , spendLimitDuration :: CardSpendLimitDuration
  , cardState          :: CardState
  , cardToken          :: PrivacyCardToken
  , cardType           :: CardType
  }
  deriving (Eq, Show)
instance FromJSON PrivacyCard where
  parseJSON = withObject "PrivacyCard" $ \o -> do
    createdTxt         <- o .: "created"
    pciCVV             <- o .:? "cvv"
    -- fundingAccount     <- o .: "funding"
    expMonthTxt        <- o .:? "exp_month"
    expYearTxt         <- o .:? "exp_year"
    hostname           <- o .: "hostname"
    lastFour           <- o .: "last_four"
    memo               <- o .: "memo"
    pciPAN             <- (RedactedText <$>) <$> o .:? "pan"
    spendLimit         <- privacyCentsToCurrency <$> o .: "spend_limit"
    spendLimitDuration <- o .: "spend_limit_duration"
    cardState          <- o .: "state"
    cardToken          <- o .: "token"
    cardType           <- o .: "type"

    created            <- case parseTime createdTxt of
      Nothing -> fail $ "PrivacyCard could not parse created " <> createdTxt
      Just u  -> return u

    pciExpiration <-
      case (readEither <$> expMonthTxt, readEither <$> expYearTxt) of
        (Nothing      , _            ) -> return Nothing
        (_            , Nothing      ) -> return Nothing
        (Just (Left e), _            ) -> fail e
        (_            , Just (Left e)) -> fail e
        (Just (Right expMonth), Just (Right expYear)) ->
          return $ Just CardExpiration { .. }

    return PrivacyCard { .. }

data TransactionResult
  -- | Card state was paused at the time of authorization
  = CardIsPaused
  -- | Card state was closed at the time of authorization
  | CardIsClosed
  -- | Platform spend limit exceeded, contact support@example.com
  | GlobalTransactionLimit
  -- | Platform spend limit exceeded, contact support@example.com
  | GlobalWeeklyLimit
  -- | Platform spend limit exceeded, contact support@example.com
  | GlobalMonthlyLimit
  -- | User-set spend limit exceeded
  | UserTransactionLimit
  -- | Merchant locked card attempted at different merchant
  | UnauthorizedMerchant
  -- | Single use card attempted multiple times
  | SingleUseRecharged
  -- | Please reconnect a funding source
  | BankConectionError
  -- | Please ensure the funding source is connected and up to date
  | InsufficientFunds
  -- | This merchant is disallowed on the platform
  | MerchantBlacklist
  -- | Incorrect CVV or expiry date
  | InvalidCardDetails
  -- | Please confirm the funding source
  | BankNotVerified
  -- | Contact support@example.com
  | InactiveAccount
  -- | Contact support@example.com
  | AccountStateTransactionFail
  -- | Network error, re-attempt the transaction
  | UnknownHostTimeout
  -- | Network error, re-attempt the transaction
  | SwitchInoperativeAdvice
  -- | Transaction declined due to risk
  | FraudAdvice
  -- | PIN verification failed
  | IncorrectPin
  -- | *Not documented*
  | UnknownError
  -- | *Not documented*
  | MalformedResponse
  -- | *Not documented*
  | GeneralDecline
  -- | Successful transaction; no reason to decline
  | Approved
  deriving (Eq, Show)
instance FromJSON TransactionResult where
  parseJSON = withText "TransactionDeclinedReason" $ \case
    "APPROVED"                   -> return Approved
    "card_a"                -> return CardIsPaused
    "card_a"                -> return CardIsClosed
    "GLOBAL_TRANSACTION_LIMIT"   -> return GlobalTransactionLimit
    "GLOBAL_WEEKLY_LIMIT"        -> return GlobalWeeklyLimit
    "GLOBAL_MONTHLY_LIMIT"       -> return GlobalMonthlyLimit
    "USER_TRANSACTION_LIMIT"     -> return UserTransactionLimit
    "UNAUTHORIZED_MERCHANT"      -> return UnauthorizedMerchant
    "SINGLE_USE_RECHARGED"       -> return SingleUseRecharged
    "BANK_CONNECTION_ERROR"      -> return BankConectionError
    "INSUFFICIENT_FUNDS"         -> return InsufficientFunds
    "INSUFFICIENT_FUNDS_PRELOAD" -> return InsufficientFunds
    "MERCHANT_BLACKLIST"         -> return MerchantBlacklist
    "INVALID_card_a"       -> return InvalidCardDetails
    "BANK_NOT_VERIFIED"          -> return BankNotVerified
    "INACTIVE_ACCOUNT"           -> return InactiveAccount
    "ACCOUNT_STATE_TRANSACTION_FAIL" -> return AccountStateTransactionFail
    "UNKNOWN_HOST_TIMEOUT"       -> return UnknownHostTimeout
    "SWITCH_INOPERATIVE_ADVICE"  -> return SwitchInoperativeAdvice
    "FRAUD_ADVICE"               -> return FraudAdvice
    "INCORRECT_PIN"              -> return IncorrectPin
    "UNKNOWN_ERROR"              -> return UnknownError -- Undocumented
    "MALFORMED_RESPONSE"         -> return MalformedResponse -- Undocumented
    "DECLINED"                   -> return GeneralDecline
    t -> fail $ "TransactionDeclinedReason doesn't recognize " <> show t

data EventType
  = Authorization
  | AuthorizationAdvice
  | Clearing
  | Void
  | Return
  deriving (Eq, Show)
instance FromJSON EventType where
  parseJSON = withText "EventType" $ \case
    "AUTHORIZATION"        -> return Authorization
    "AUTHORIZATION_ADVICE" -> return AuthorizationAdvice
    "CLEARING"             -> return Clearing
    "VOID"                 -> return Void
    "RETURN"               -> return Return
    t                      -> fail $ "EventType does not recognize " <> show t

newtype EventToken = EventToken Text deriving (Eq, Show)

data TransactionEvent = TransactionEvent
  { amount      :: Currency
  , created     :: UTCTime
  , eventResult :: TransactionResult
  , eventType   :: EventType
  , eventToken  :: EventToken
  }
  deriving (Eq, Show)
instance FromJSON TransactionEvent where
  parseJSON = withObject "TransactionEvent" $ \o -> do
    rawAmount   <- privacyCentsToCurrency <$> o .: "amount"
    createdTxt  <- o .: "created"
    eventResult <- o .: "result"
    eventToken  <- EventToken <$> o .: "token"
    eventType   <- o .: "type"
    let amount = case eventType of
          Authorization       -> rawAmount
          AuthorizationAdvice -> rawAmount
          Clearing            -> rawAmount
          Void                -> rawAmount * (-1)
          Return              -> rawAmount * (-1)

    created <- case parseTime createdTxt of
      Nothing ->
        fail $ "TransactionEvent could not parse created " <> createdTxt
      Just u -> return u

    return TransactionEvent { .. }

data Merchant = Merchant
  { acceptorId :: Text
  , city       :: Text
  , country    :: Text
  , descriptor :: Text
  , mcc        :: Int
  , state      :: Text
  }
  deriving (Eq, Show)
instance FromJSON Merchant where
  parseJSON = withObject "Merchant" $ \o -> do
    acceptorId <- o .: "acceptor_id"
    city       <- o .: "city"
    country    <- o .: "country"
    descriptor <- o .: "descriptor"
    mccStr     <- o .: "mcc"
    state      <- o .: "state"

    mcc        <- case readEither mccStr of
      Left  e   -> fail e
      Right int -> return int

    return Merchant { .. }

newtype TransactionFundingToken = TransactionFundingToken Text deriving (Eq, Show)

data TransactionFunding = TransactionFunding
  { amount  :: Currency
  , tfToken :: TransactionFundingToken
  , tfType  :: Text
  }
  deriving (Eq, Show)
instance FromJSON TransactionFunding where
  parseJSON = withObject "TransactionFunding" $ \o -> do
    amount  <- privacyCentsToCurrency <$> o .: "amount"
    tfToken <- TransactionFundingToken <$> o .: "token"
    tfType  <- o .: "type"

    return TransactionFunding { .. }

data TransactionStatus
  -- | Authorization is pending completion from the merchant
  = PendingTrx
  -- | The merchant has voided the previously pending authorization
  | VoidedTrx
  -- | The merchant has completed the transaction and the funding source is being debited
  | SettlingTrx
  -- | The transaction is complete
  | SettledTrx
  -- | There was an error settling the transaction against the funding source. Your API account may be disabled
  | BoundedTrx
  -- | The transaction was declined
  | DeclinedTrx TransactionResult
  deriving (Eq, Show)

newtype TransactionToken = TransactionToken Text deriving (Eq, Show)
instance FromJSON TransactionToken where
  parseJSON = withText "TransactionToken" $ return . TransactionToken
instance ToJSON TransactionToken where
  toJSON (TransactionToken t) = toJSON t

data Transaction = Transaction
  { amount        :: Currency
  , card          :: PrivacyCard
  , created       :: UTCTime
  , events        :: [TransactionEvent]
  -- , funding       :: Maybe [TransactionFunding]
  , merchant      :: Merchant
  , result        :: TransactionResult
  , settledAmount :: Currency
  , trxStatus     :: TransactionStatus
  , trxToken      :: TransactionToken
  }
  deriving (Eq, Show)
instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    amount            <- privacyCentsToCurrency <$> o .: "amount"
    card              <- o .: "card"
    createdTxt        <- o .: "created"
    events            <- o .: "events"
    -- funding           <- o .: "funding"
    merchant          <- o .: "merchant"
    result            <- o .: "result"
    settledAmount     <- privacyCentsToCurrency <$> o .: "settled_amount"
    statusTxt :: Text <- o .: "status"
    trxToken          <- o .: "token"

    created           <- case parseTime createdTxt of
      Nothing -> fail $ "Transaction could not parse created " <> createdTxt
      Just u  -> return u

    trxStatus <- case statusTxt of
      "PENDING"  -> return PendingTrx
      "VOIDED"   -> return VoidedTrx
      "SETTLING" -> return SettlingTrx
      "SETTLED"  -> return SettledTrx
      "BOUNCED"  -> return BoundedTrx
      "DECLINED" -> return $ DeclinedTrx result
      t          -> fail $ "TransactionStatus does not recognize " <> show t

    return Transaction { .. }

data AccountSpendLimits = AccountSpendLimits
  { accountToken       :: PrivacyAccountToken
  , spendLimitDaily    :: Currency
  , spendLimitLifetime :: Currency
  , spendLimitMonthly  :: Currency
  }
  deriving (Eq, Show)
instance FromJSON AccountSpendLimits where
  parseJSON = withObject "AccountSpendLimits" $ \o -> do
    accountToken       <- o .: "account_token"
    spendLimits        <- o .: "spend_limit" <?> Key "account_token"
    spendLimitDaily    <- privacyCentsToCurrency <$> spendLimits .: "daily"
    spendLimitLifetime <- privacyCentsToCurrency <$> spendLimits .: "lifetime"
    spendLimitMonthly  <- privacyCentsToCurrency <$> spendLimits .: "monthly"

    return AccountSpendLimits { .. }

privacyCentsToCurrency :: Int -> Currency
privacyCentsToCurrency cents = Currency "USD" $ toRational cents / 100
