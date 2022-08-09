{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.Plaid.Webhook where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics
import           Shared.Models.Plaid.Base

data WebhookType
  = Auth AuthWebhook
  | Transactions TransactionWebhooks
  | ItemWH ItemWebhooks
  | Income IncomeWebhooks
  | Assets AssetsWebhooks
  | Unknown Value
  deriving (Eq, Show, Generic)
instance FromJSON WebhookType where
  parseJSON = withObject "WebhookType" $ \a -> do
    hookType :: Text <- a .: "webhook_type"
    case hookType of
      "AUTH"         -> Auth <$> parseJSON (Object a)
      "TRANSACTIONS" -> Transactions <$> parseJSON (Object a)
      "ITEM"         -> ItemWH <$> parseJSON (Object a)
      "INCOME"       -> Income <$> parseJSON (Object a)
      "ASSETS"       -> Assets <$> parseJSON (Object a)
      _              -> return $ Unknown (Object a)

data AuthWebhook
  = AutomaticallyVerified { acItemId :: ItemId, acAccountId :: Text }
  | VerificationExpired { veItemId :: ItemId, veAccountId :: Text, veError :: Value }
  | UnknownAuth Value
  deriving (Eq, Show)

-- parseAuthWebhook :: Object -> Parser AuthWebhook
-- parseAuthWebhook o = do
instance FromJSON AuthWebhook where
  parseJSON = withObject "AuthWebhook" $ \a -> do
    hookCode :: Text <- a .: "webhook_code"
    case hookCode of
      "AUTOMATICALLY_VERIFIED" ->
        AutomaticallyVerified <$> a .: "item_id" <*> a .: "account_id"
      "ERROR" ->
        VerificationExpired
          <$> (a .: "item_id")
          <*> (a .: "account_id")
          <*> (a .: "error")
      _ -> return $ UnknownAuth (Object a)

data TransactionWebhooks
  = InitialUpdate { icItemID :: ItemId, icNewTransactions :: Integer }
  | HistoricalUpdate { huItemID :: ItemId, huNewTransactions :: Integer }
  | DefaultUpdate { duItemID :: ItemId, duNewTransactions :: Integer }
  | TransactionsRemoved { trItemID :: ItemId, trRemovedTransactions :: [Text] }
  | UnknownTransaction Value
  deriving (Eq, Show)
instance FromJSON TransactionWebhooks where
  parseJSON = withObject "TransactionWebhooks" $ \a -> do
    hookCode :: Text <- a .: "webhook_code"
    case hookCode of
      "INITIAL_UPDATE" ->
        InitialUpdate <$> a .: "item_id" <*> a .: "new_transactions"
      "HISTORICAL_UPDATE" ->
        HistoricalUpdate <$> a .: "item_id" <*> a .: "new_transactions"
      "DEFAULT_UPDATE" ->
        DefaultUpdate <$> a .: "item_id" <*> a .: "new_transactions"
      "TRANSACTIONS_REMOVED" ->
        TransactionsRemoved <$> a .: "item_id" <*> a .: "removed_transactions"
      _ -> return $ UnknownTransaction (Object a)

data ItemErrors
  = InvalidCredentials
  | InvaildMFA
  | InvalidSendMethod
  | InvalidUpdatedUsername
  | ItemLocked
  | ItemLoginRequired
  | ItemNoError
  | ItemNoVerification
  | IncorrectDepositAmounts
  | TooManyVerificationAttempts
  | UserSetupRequired
  | MFANotSupported
  | NoAccounts
  | NoAuthAccounts
  | NoInvestmentAccounts
  | ProductNotReady
  | ProductsNotSupported
  | UnknownItemError (Maybe Text)
  deriving (Eq, Show)
instance FromJSON ItemErrors where
  parseJSON = withObject "ItemErrors" $ \a -> do
    errorCode :: Text          <- a .: "error_code"
    errorMessage :: Maybe Text <- a .:? "error_message"
    return $ case errorCode of
      "INVALID_CREDENTIALS" -> InvalidCredentials
      "INVALID_MFA"         -> InvaildMFA
      "ITEM_LOGIN_REQUIRED" -> ItemLoginRequired
      _                     -> UnknownItemError errorMessage

data ItemWebhooks
  = WebhookUpdateAcknowledge { wiItemID :: ItemId, wiURL :: Text }
  | ItemError { ieItemID :: ItemId, ieError :: ItemErrors }
  | UnknownItem Value
  deriving (Eq, Show)
instance FromJSON ItemWebhooks where
  parseJSON = withObject "ItemErrors" $ \a -> do
    hookCode :: Text <- a .: "webhook_code"
    case hookCode of
      "WEBHOOK_UPDATE_ACKNOWLEDGED" ->
        WebhookUpdateAcknowledge <$> a .: "item_id" <*> a .: "new_webhook_url"
      "ERROR" -> ItemError <$> a .: "item_id" <*> a .: "error"
      _       -> return $ UnknownItem (Object a)

data IncomeWebhooks
  = IncomeProductReady { iprItemID :: ItemId }
  | UnknownIncome Value
  deriving (Eq, Show)
instance FromJSON IncomeWebhooks where
  parseJSON = withObject "ItemErrors" $ \a -> do
    hookCode :: Text <- a .: "webhook_code"
    case hookCode of
      "PRODUCT_READY" -> IncomeProductReady <$> a .: "item_id"
      _               -> return $ UnknownIncome (Object a)

data AssetsWebhooks
  = AssetsProductReady { aprAssetReportID :: ItemId }
  | AssetReportError { areAssetReportID :: ItemId }
  | UnknownAssets Value
  deriving (Eq, Show)
instance FromJSON AssetsWebhooks where
  parseJSON = withObject "ItemErrors" $ \a -> do
    hookCode :: Text <- a .: "webhook_code"
    case hookCode of
      "PRODUCT_READY" -> AssetsProductReady <$> a .: "asset_report_id"
      "ERROR"         -> AssetReportError <$> a .: "asset_report_id"
      _               -> return $ UnknownAssets (Object a)

