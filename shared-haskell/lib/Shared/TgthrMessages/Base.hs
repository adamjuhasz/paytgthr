{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Shared.TgthrMessages.Base
  ( module Shared.TgthrMessages.Base
  , MessageID(..)
  ) where

import           Data.Aeson                    as A
                                                ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.ByteString.Char8         as BC8
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(Escape)
                                                , ToField(..)
                                                )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Ids              ( MessageID(..) )

class ThroughMQ a where
  toKey :: a -> Text

fromMessageID :: MessageID -> UUID
fromMessageID (MessageID u) = u

data AccountType
  = Depository DepositoryType
  | Credit CreditType
  | Loan
  | Mortgage
  | Brokerage Text
  | Other
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data DepositoryType
  = Checking
  | Savings
  | Prepaid
  | DepositoryTypeUnknown
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data CreditType
  = CreditCard
  | CreditTypeUnknown
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

instance ToField AccountType where
  toField acct = Escape $ BC8.pack $ show acct
instance FromField AccountType where
  fromField a dat = read <$> fromField a dat
