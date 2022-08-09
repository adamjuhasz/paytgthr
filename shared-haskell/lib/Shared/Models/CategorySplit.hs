{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.CategorySplit where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import qualified Database.PostgreSQL.Simple.Types
                                               as PT
import           GHC.Generics                   ( Generic )
import           Shared.Models.Group            ( GroupId
                                                , GroupSplit
                                                )

data CategoryCode
  = Category000 -- Default Split
  | Category001
  | Category002
  | Category003
  | Category004
  | Category005
  | Category006
  deriving (Eq, Show, Read, Ord, Generic)
instance ToJSON CategoryCode where
instance FromJSON CategoryCode where
instance ToField CategoryCode where
  toField = toField . show
instance FromField CategoryCode where
  fromField a dat = read <$> fromField a dat

categoryDescription :: CategoryCode -> Text
categoryDescription Category000 = "Everything else"
categoryDescription Category001 = "Gas Stations & Transportation"
categoryDescription Category002 = "Grocery & Big Box Stores"
categoryDescription Category003 = "Media Subscriptions"
categoryDescription Category004 = "Pet Food and Supplies"
categoryDescription Category005 = "Restaurants, Takeout & Bars"
categoryDescription Category006 =
  "Utilities, Internet, Cable, Phone and Insurance"

data CategoryState
  = CategoryActive
  | CategoryDisabled
  deriving (Eq, Show, Read, Generic)
instance ToJSON CategoryState where
instance FromJSON CategoryState where
instance ToField CategoryState where
  toField = toField . show
instance FromField CategoryState where
  fromField a dat = read <$> fromField a dat

data CategorySplit = CategorySplit
  { groupId     :: GroupId
  , categoryId  :: CategoryCode
  , catRevision :: Int
  , createdAt   :: UTCTime
  , updatedAt   :: UTCTime
  , splits      :: [GroupSplit]
  , state       :: CategoryState
  }
  deriving (Eq, Show, Generic)
instance ToJSON CategorySplit where
instance FromJSON CategorySplit where

instance ToRow CategorySplit where
  toRow CategorySplit {..} =
    [ toField groupId     -- UUID
    , toField categoryId  -- Text
    , toField catRevision -- Int
    , toField createdAt   -- TimeZone
    , toField updatedAt   -- TimeZone
    , toField splits      -- JSONB
    , toField state       -- Text
    ]

instance FromRow CategorySplit where
  fromRow = do
    groupId     <- field
    categoryId  <- field
    catRevision <- field
    createdAt   <- field
    updatedAt   <- field
    splits      <- field
    state       <- field

    return CategorySplit { .. }

categorySplitFields :: (PT.Query, PT.Query)
categorySplitFields =
  ( " group_id, category_id, revision, created_at, updated_at, split, state "
  , " ?       , ?          , ?       , ?         , ?         , ?    , ?     "
  )
