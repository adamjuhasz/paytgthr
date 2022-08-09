{-# LANGUAGE DeriveGeneric, RecordWildCards, StrictData  #-}
module Shared.Models.LinkedAccount where

import           GHC.Generics
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Shared.Models.User


data LinkedAccount
  = DirectACH -- Plaid
    { linkedUser :: UserID
    , abaRouting :: Text
    , ddaNumber :: Text
    , bankName :: Text
    , accountName :: Text
    , dwollaFSId :: Text
    }
  | ManualACH -- manually entered
    { linkedUser :: UserID
    , abaRouting :: Text
    , ddaNumber :: Text
    , bankName :: Text
    , accountName :: Text
    , dwollaFSId :: Text
    , verified :: Bool
    }
  deriving (Eq, Show, Generic)

instance ToRow LinkedAccount where
  toRow DirectACH {..} =
    [ toField ("DirectACH" :: Text) -- "type"
    , toField linkedUser -- "user"
    , toField abaRouting -- "aba"
    , toField ddaNumber -- "dda"
    , toField bankName -- "bank"
    , toField accountName -- "account"
    , toField dwollaFSId -- "token"
    , toField (Nothing :: Maybe Bool) -- "verified"
    ]
  toRow ManualACH {..} =
    [ toField ("ManualACH" :: Text) -- "type"
    , toField linkedUser -- "user"
    , toField abaRouting -- "aba"
    , toField ddaNumber -- "dda"
    , toField bankName -- "bank"
    , toField accountName -- "account"
    , toField dwollaFSId -- "token"
    , toField $ Just verified -- "verified"
    ]
