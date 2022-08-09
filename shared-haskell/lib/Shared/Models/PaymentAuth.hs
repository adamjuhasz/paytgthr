{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, OverloadedStrings, RecordWildCards, ScopedTypeVariables, StrictData  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shared.Models.PaymentAuth
  ( module Shared.Models.PaymentAuth
  , UserID
  , MessageID
  , Currency
  , PlaidAccountId
  , ItemId
  , AccessToken
  , Account
  , GroupId
  ) where

import           Control.Monad                  ( ap )
import           Data.Aeson                     ( ToJSON )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Database.PostgreSQL.Simple.FromField
                                               as PQ
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow
                                                , field
                                                , fromRow
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(Escape)
                                                , ToField(..)
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow
                                                , toRow
                                                )
import qualified Database.PostgreSQL.Simple.Types
                                               as PT
import           GHC.Generics                   ( Generic )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                )
import           Shared.Models.Group            ( GroupId )
import           Shared.Models.Plaid.Base       ( AccessToken(..)
                                                , Account(..)
                                                , ItemId(..)
                                                , PlaidAccountId(..)
                                                , PlaidEnvironment(..)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )

instance ToField PlaidAccountId where
  toField (PlaidAccountId theID) = Escape $ encodeUtf8 theID
instance PQ.FromField PlaidAccountId where
  fromField a dat = PlaidAccountId <$> PQ.fromField a dat

instance ToField AccessToken where
  toField (AccessToken uuid) = toField uuid
instance PQ.FromField AccessToken where
  fromField a dat = AccessToken <$> PQ.fromField a dat

plaidEnvFromStr :: Text -> PlaidEnvironment
plaidEnvFromStr "sandbox"     = Sandbox
plaidEnvFromStr "development" = Development
plaidEnvFromStr "production"  = Production
plaidEnvFromStr _             = Prelude.error "Don't understand this plaid env"

instance ToField PlaidEnvironment where
  toField Sandbox     = Escape "sandbox"
  toField Development = Escape "development"
  toField Production  = Escape "production"
instance PQ.FromField PlaidEnvironment where
  fromField a dat = plaidEnvFromStr <$> PQ.fromField a dat

instance ToField ItemId where
  toField (ItemId uuid) = toField uuid
instance PQ.FromField ItemId where
  fromField a dat = ItemId <$> PQ.fromField a dat

data PlaidTokenRow = PlaidTokenRow
  { aToken         :: AccessToken
  , userId         :: UserID
  , revision       :: Integer
  , accountPrimary :: Maybe PlaidAccountId
  , accountABA     :: Maybe Text
  , accountDDA     :: Maybe Text
  , plaidEnv       :: PlaidEnvironment
  , msgSource      :: MessageID
  , itemId         :: ItemId
  }
  deriving (Eq, Show, Generic, ToRow, FromRow, ToJSON)

defaultPlaidTokenRow
  :: AccessToken
  -> UserID
  -> PlaidEnvironment
  -> MessageID
  -> ItemId
  -> PlaidTokenRow
defaultPlaidTokenRow atok uid env mid iid = PlaidTokenRow
  { aToken         = atok
  , userId         = uid
  , revision       = 1
  , accountPrimary = Nothing
  , accountABA     = Nothing
  , accountDDA     = Nothing
  , plaidEnv       = env
  , msgSource      = mid
  , itemId         = iid
  }

plaidTokenRowFieldOrder :: (PT.Query, PT.Query)
plaidTokenRowFieldOrder =
  ( " token, user_id, revision, account_primary, account_primary_routing, account_primary_account, plaid_environment, msg_source, plaid_item_id "
  , " ?, ?, ?, ?, ?, ?, ?, ?, ? "
  )

instance ToJSON Account

data PlaidBalanceRow = PlaidBalanceRow
  { pbUserId     :: UserID
  , pbAccessTime :: Double
  , pbMsgSource  :: MessageID
  , pbAccount    :: Account
  }
  deriving (Eq, Show, Generic, ToJSON)

currencyToDouble :: Currency -> Double
currencyToDouble = fromRational . getMonetaryValue

doubleToCurrency :: Rational -> Currency
doubleToCurrency = Currency "USD"

instance ToRow PlaidBalanceRow where
  toRow PlaidBalanceRow { pbAccount = Account {..}, ..} =
    [ toField pbUserId
    , toField pbAccessTime
    , toField pbMsgSource
    , toField accountId
    , toField accountType
    , toField (currencyToDouble accountCurrentBalance)
    , toField (currencyToDouble <$> accountAvailableBalance)
    , toField accountName
    ]
instance FromRow PlaidBalanceRow where
  fromRow =
    PlaidBalanceRow
      <$>  field
      <*>  field
      <*>  field
      <*>  return Account
      `ap` field
      `ap` field
      `ap` (doubleToCurrency <$> field)
      `ap` ((doubleToCurrency <$>) <$> field)
      `ap` return Nothing
      `ap` field
      `ap` return Nothing

plaidBalanceRowFieldOrder :: (PT.Query, PT.Query)
plaidBalanceRowFieldOrder =
  ( " user_id, access_time_sec, msg_source, account_id, account_type, balance_current, balance_available, account_name "
  , " ?, ?, ?, ?, ?, ?, ?, ? "
  )


