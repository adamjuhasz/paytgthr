{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, RecordWildCards, StrictData #-}

module Shared.Models.Plaid.Base where

import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , Object
                                                , ToJSON(..)
                                                , Value(String)
                                                , genericParseJSON
                                                , genericToJSON
                                                , withObject
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Currency         ( Currency(..)
                                                , simplifiedRational
                                                )
import           Shared.TgthrMessages.Base      ( AccountType(..)
                                                , CreditType(..)
                                                , DepositoryType(..)
                                                )
import           Shared.Utils                   ( customAesonOptions )

newtype AccessToken = AccessToken Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype ItemId = ItemId Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype PlaidAccountId = PlaidAccountId Text deriving (Eq, Show, Generic, FromJSON, ToJSON)

data PlaidEnvironment
  = Sandbox
  | Development
  | Production
  deriving (Eq, Show, Generic)
instance FromJSON PlaidEnvironment where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON PlaidEnvironment where
  toJSON = genericToJSON customAesonOptions

newtype PublicToken = PublicToken Text deriving (Eq, Show, Generic, FromJSON, ToJSON)


data Account = Account
  { accountId               :: PlaidAccountId
  , accountType             :: AccountType
  , accountCurrentBalance   :: Currency
  , accountAvailableBalance :: Maybe Currency
  , accountLimit            :: Maybe Currency
  , accountName             :: Maybe Text
  , accountOfficialName     :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Account where
  parseJSON = withObject "account" $ \a -> do
    accountId                 <- a .: "account_id"
    accountType               <- parseAccountType a
    accountName               <- a .: "name"
    accountOfficialName       <- a .: "official_name"
    balances                  <- a .: "balances"
    currencyCode              <- balances .: "iso_currency_code"
    curBalance :: Double      <- balances .: "current"
    avBalance :: Maybe Double <- balances .:? "available"
    limitBal :: Maybe Double  <- balances .:? "limit"

    let currBuilder           = Currency currencyCode
    let accountCurrentBalance = currBuilder . simplifiedRational $ curBalance
    let accountAvailableBalance =
          currBuilder . simplifiedRational <$> avBalance
    let accountLimit = currBuilder . simplifiedRational <$> limitBal

    return Account { .. }

parseAccountType :: Object -> Parser AccountType
parseAccountType o = do
  String type' <- o .: "type"
  subtype      <- parseAccountSubType <$> o .:? "subtype"

  case (type', subtype) of
    ("depository", Just "checking"   ) -> return $ Depository Checking
    ("depository", Just "savings"    ) -> return $ Depository Savings
    ("depository", Just "prepaid"    ) -> return $ Depository Prepaid
    ("depository", _) -> return $ Depository DepositoryTypeUnknown
    ("credit"    , Just "credit card") -> return $ Credit CreditCard
    ("credit"    , _                 ) -> return $ Credit CreditTypeUnknown
    ("brokerage" , Just aSubtype     ) -> return $ Brokerage aSubtype
    ("brokerage" , Nothing           ) -> return $ Brokerage "unknown"
    (_           , _                 ) -> return Other

parseAccountSubType :: Maybe Value -> Maybe Text
parseAccountSubType (Just (String t)) = Just t
parseAccountSubType _                 = Nothing

data PlaidError = PlaidError
  { error_type      :: Text
  , error_code      :: Text
  , error_message   :: Text
  , display_message :: Maybe Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON PlaidError where
  parseJSON = withObject "PlaidError" $ \o -> do
    error_type      <- o .: "error_type"
    error_code      <- o .: "error_code"
    error_message   <- o .: "error_message"
    display_message <- o .:? "display_message"

    return PlaidError { .. }

data Item = Item
  { itemId                :: ItemId
  , itemInstitutionId     :: Text
  , itemWebhook           :: Maybe Text
  , itemError             :: Maybe PlaidError
  , itemAvailableProducts :: [Text]
  , itemBilledProducts    :: [Text]
  }
  deriving (Eq, Show)
instance FromJSON Item where
  parseJSON = withObject "Item" $ \o ->
    Item
      <$> (o .: "item_id")
      <*> (o .: "institution_id")
      <*> (o .: "webhook")
      <*> (o .: "error")
      <*> (o .: "available_products")
      <*> (o .: "billed_products")

data BalanceResponse = BalanceResponse
  { balAccounts  :: [Account]
  , balItem      :: Item
  , balRequestId :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON BalanceResponse where
  parseJSON = withObject "BalanceResponse" $ \o -> do
    balAccounts  <- o .: "accounts"
    balItem      <- o .: "item"
    balRequestId <- o .: "request_id"
    return BalanceResponse { .. }

data ACH = ACH
  { achAccountId   :: PlaidAccountId
  , achDDANumber   :: Text
  , achABARouting  :: Text
  , achWireRouting :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON ACH where
  parseJSON = withObject "ach" $ \a ->
    ACH
      <$> (a .: "account_id")
      <*> (a .: "account")
      <*> (a .: "routing")
      <*> (a .:? "wire_routing")

newtype Numbers = Numbers
  { ach :: [ACH]
  } deriving (Eq, Show, Generic, FromJSON)

data AuthResponse = AuthResponse
  { authAccounts  :: [Account]
  , authNumbers   :: Numbers
  , authItem      :: Item
  , authRequestId :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON AuthResponse where
  parseJSON = withObject "AuthResponse" $ \o -> do
    authAccounts  <- o .: "accounts"
    authNumbers   <- o .: "numbers"
    authItem      <- o .: "item"
    authRequestId <- o .: "request_id"
    return AuthResponse { .. }

data ExhangeTokenResponse = ExhangeTokenResponse
  { etrAccessToken :: AccessToken
  , etrItemId      :: ItemId
  , etrRequestId   :: Text
  }
  deriving (Eq, Show)
instance FromJSON ExhangeTokenResponse where
  parseJSON = withObject "ExhangeTokenResponse" $ \o ->
    ExhangeTokenResponse
      <$> (o .: "access_token")
      <*> (o .: "item_id")
      <*> (o .: "request_id")
