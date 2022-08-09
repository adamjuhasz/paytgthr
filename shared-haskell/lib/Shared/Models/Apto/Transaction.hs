{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass  #-}
{-# LANGUAGE StrictData, RecordWildCards #-}

module Shared.Models.Apto.Transaction
  ( module Shared.Models.Apto.Transaction
  , TransactionSource(..)
  , TransactionDetails(..)
  , MerchantInfo(..)
  , CardDispute(..)
  , TransactionState(..)
  ) where

import           Control.Applicative            ( Alternative((<|>)) )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (<?>)
                                                , FromJSON(parseJSON)
                                                , Object
                                                , ToJSON
                                                , Value
                                                , withObject
                                                )
import           Data.Aeson.Types               ( JSONPathElement(Key)
                                                , Parser
                                                )
import           Data.Foldable                  ( asum )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , isInfixOf
                                                , toLower
                                                )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Apto.Base        ( AptoCardholderId )
import           Shared.Models.Card             ( AptoCardId )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Transaction     as Trx
                                                ( AptoAdjustment(..)
                                                , CardDispute(..)
                                                , DeclineReason(..)
                                                , MerchantInfo(..)
                                                , PurchaseType(..)
                                                , TransactionDetails(..)
                                                , TransactionEvent(..)
                                                , TransactionSource(..)
                                                , TransactionState(..)
                                                )

newtype AptoTransactionId = AptoTransactionId Text deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AptoCardHolder = AptoCardHolder
  { apcId        :: AptoCardholderId
  , apcEmail     :: Text
  , apcNameFirst :: Text
  , acpNameLast  :: Text
  }
  deriving (Eq, Show, Generic)

parseAdjustment :: Value -> Parser AptoAdjustment
parseAdjustment = withObject "AptoAdjustment" $ \o -> do
  adj                <- o .: "adjustment"
  adjId              <- adj .: "id" <?> Key "adjustment"
  adjCreatedAt       <- adj .: "created_at" <?> Key "adjustment"
  adjAmountLocal     <- extractCurr adj "local_amount" <?> Key "adjustment"
  adjAmountBilling   <- extractCurr adj "billing_amount" <?> Key "adjustment"
  adjFSTransactionId <- adj .: "funding_source_transaction_id" <?> Key
    "adjustment"
  adjType <- adj .: "type" <?> Key "adjustment"
  return AptoAdjustment { .. }

data AptoTransaction = AptoTransaction
  { aptSource :: TransactionSource
  , aptIdempotency :: Maybe Text
  , aptSourceId :: AptoTransactionId
  , aptCardId :: AptoCardId
  , aptDetails :: TransactionDetails
  , aptCardholder :: AptoCardHolder
  , aptMerchant :: MerchantInfo
  , aptStandin :: Bool
  , aptState :: TransactionState
  , aptCreatedAt :: UTCTime
  , aptTransactionEvent :: TransactionEvent
  , aptAmountLocal :: Currency
  , aptAmountHold :: Currency
  , aptAmountCashback :: Currency
  , aptAmountFee :: Currency
  , aptAmountBilling :: Currency
  , aptVersion :: Text
  , aptAdjustments :: [AptoAdjustment]
  }
  | CardNotification -- Notification that something has happened witht the card
  { notSource :: TransactionSource
  , notSourceId :: AptoTransactionId
  , notCardId :: AptoCardId
  , notCardholder :: AptoCardHolder
  , notContext :: Text
  }
  deriving (Eq, Show, Generic)

emptyStringToNull :: Maybe Text -> Maybe Text
emptyStringToNull (Just "") = Nothing
emptyStringToNull (Just t ) = Just t
emptyStringToNull _         = Nothing

declineCodeConv :: Maybe Text -> Text -> DeclineReason
declineCodeConv _      "decline_nsf"            = LowBalance []
declineCodeConv _      "decline_03"             = InvalidMerchant
declineCodeConv _      "decline_04"             = LostorStolenCard
declineCodeConv _      "decline_13"             = InvalidAmount
declineCodeConv _      "decline_14"             = InvalidCardNumber
declineCodeConv _      "decline_33"             = CardExpired
declineCodeConv _      "decline_34"             = SuspectFraud
declineCodeConv _      "decline_41"             = LostorStolenCard
declineCodeConv _      "decline_42"             = InternationalNotAllowed
declineCodeConv _      "decline_43"             = LostorStolenCard
declineCodeConv _      "decline_54"             = CardExpired
declineCodeConv _      "decline_59"             = SuspectFraud
declineCodeConv _      "decline_75"             = PinRetriesExceeded
declineCodeConv _      "decline_86"             = PinRetriesExceeded
declineCodeConv _      "decline_bad_cvv_or_exp" = IncorrectCVV
declineCodeConv _      "decline_GV"             = IncorrectCVV
declineCodeConv _      "decline_pre_active"     = CardNotActivated
declineCodeConv _      "decline_inactive_card"  = CardInactive
declineCodeConv _      "decline_closed_card"    = Trx.CardClosed
declineCodeConv _      "decline_X1"             = IncorrectAddress
declineCodeConv _      "decline_bad_pin"        = IncorrectPin
declineCodeConv reason x                        = Unknown $ fromMaybe x reason
instance FromJSON AptoTransaction where
  parseJSON = withObject "AptoTransaction" $ \o -> do
    trx         <- o .: "transaction"
    aptoState   <- toLower <$> trx .: "state" <?> Key "transaction"
    aptoType    <- toLower <$> trx .: "type" <?> Key "transaction"
    aptoContext <- trx .: "context" <?> Key "transaction"

    -- Decline
    let genricDecline = Unknown ""
    declineReason :: Maybe Text <- trx .:? "decline_reason" <?> Key
      "transaction"
    declinedTrx <-
      (trx .:? "decline_code" <?> Key "transaction")
      <&> (\c -> c <&> declineCodeConv declineReason)
      <&> fromMaybe genricDecline
      <&> TrxDeclined

    let notRecognized x = fail $ errorMes aptoState aptoType <> " for " <> x

    aptoDispute <- asum
      [ Just
        <$> (DisputeLost <$> trx .: "dispute_id" <*> trx .: "dispute_lost_at")
      , Just
        <$> (DisputeWon <$> trx .: "dispute_id" <*> trx .: "dispute_won_at")
      , Just <$> (Disputing <$> trx .: "dispute_id" <*> trx .: "dispute_at")
      , return Nothing
      ]

    aptState <- case (aptoDispute, aptoState, aptoType) of
      (Just d, _, _) -> return $ TrxDisputed d
      (_, "pending", "reversal") -> return TrxPendingReversal
      (_, "pending", _) -> return TrxPending
      (_, "declined", _) -> return declinedTrx
      (_, "complete", _) -> return TrxCompleted
      _ -> notRecognized "aptState"

    aptTransactionEvent <- case (aptoState, aptoType) of
      (_         , "non_financial"  ) -> return NonFinancialEvent
      ("pending" , "pending"        ) -> return StateTransition
      ("pending" , "purchase"       ) -> return StateTransition
      ("pending" , "pin_purchase"   ) -> return StateTransition
      ("declined", "decline"        ) -> return StateTransition
      ("complete", "purchase"       ) -> return StateTransition
      ("complete", "pin_purchase"   ) -> return StateTransition
      ("complete", "pending"        ) -> return AuthRequest
      ("complete", "reversal"       ) -> return Reversal
      ("complete", "refund"         ) -> return Refund
      ("complete", "credit"         ) -> return NetworkCredit
      ("complete", "debit"          ) -> return NetworkDebit
      ("complete", "balance_inquiry") -> return BalanceInquiry
      ("complete", "unsupported"    ) -> return NonFinancialEvent
      _                               -> return UnknownTransactionEvent

    let isNotification = case aptTransactionEvent of
          NonFinancialEvent ->
            ("Token" `isInfixOf` aptoContext)         -- This is for Digital Wallets (Apple pay / Google pay)
                                              || True -- This is broad
          _ -> False

    -- AptoCardHolder
    apcId        <- trx .: "cardholder_id" <?> Key "transaction"
    apcEmail     <- trx .: "cardholder_email" <?> Key "transaction"
    apcNameFirst <- trx .: "cardholder_first_name" <?> Key "transaction"
    acpNameLast  <- trx .: "cardholder_last_name" <?> Key "transaction"

    if isNotification
      then do
        let notSource     = Apto
        let notCardholder = AptoCardHolder { .. }
        let notContext    = aptoContext
        notSourceId <- trx .: "id" <?> Key "transaction"
        notCardId   <- trx .: "card_a" <?> Key "transaction"
        return CardNotification { .. }
      else do
        aptAmountLocal    <- extractCurr trx "local_amount"
        aptAmountHold     <- extractCurr trx "hold_amount"
        aptAmountCashback <- extractCurr trx "cashback_amount"
        aptAmountFee      <- extractCurr trx "fee_amount"
        aptAmountBilling  <- extractCurr trx "billing_amount"
        aptIdempotency    <- trx .: "idempotency_key" <?> Key "transaction"
        aptSourceId       <- trx .: "id" <?> Key "transaction"
        aptCardId         <- trx .: "card_a" <?> Key "transaction"
        aptStandin        <- trx .: "shift_stand_in" <|> o .: "apto_stand_in"
        aptCreatedAt      <- trx .: "created_at" <?> Key "transaction"
        aptAdjustments    <- trx .: "adjustments" >>= mapM parseAdjustment

        -- CardTransaction
        let pcpType = case aptoType of
              "pin_purchase" -> Just Pin
              "purchase"     -> Just Signature
              _              -> Nothing
        let pcpContext = aptoContext
        pcpIsCardPresent   <- trx .: "card_a" <?> Key "transaction"
        pcpIsOnline        <- trx .: "ecommerce" <?> Key "transaction"
        pcpIsInternational <- trx .: "international" <?> Key "transaction"
        pcpNetwork         <- trx .: "network" <?> Key "transaction"
        pcpIsEMV           <- trx .: "emv" <?> Key "transaction"
        pcpDescription     <-
          trx .: "description" <?> Key "transaction" <&> emptyStringToNull

        -- CardMerchant
        cmiMcc      <- trx .: "mcc"
        cmiMccDesc  <- trx .: "mcc_group" <|> trx .: "mcc_name" <|> return ""
        cmiName     <- trx .: "merchant_name"
        cmiLocality <- trx .:? "merchant_locality" <&> emptyStringToNull
        cmiRegion   <- trx .: "merchant_region" <&> emptyStringToNull
        cmiCountry  <- trx .: "merchant_country"

        let aptSource     = Apto
            aptVersion    = "1.0"
            aptDetails    = CardTransaction { .. }
            aptMerchant   = CardMerchant { .. }
            aptCardholder = AptoCardHolder { .. }
        return AptoTransaction { .. }
   where
    errorMes st ty =
      "Unknown (state, type) combo (" <> show st <> ", " <> show ty <> ")"

extractCurr :: Object -> Text -> Parser Currency
extractCurr obj key = obj .: key >>= \sub ->
  Currency
    <$> (sub .: "currency")
    <*> (toRational <$> (sub .: "amount" :: Parser Double))
