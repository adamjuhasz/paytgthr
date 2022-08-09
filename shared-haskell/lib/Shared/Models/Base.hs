{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances, StrictData #-}

module Shared.Models.Base
  ( module Shared.Models.Base
  , UserID(..)
  ) where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Shared.Models.Ids              ( UserID(..) )

type Revision = Int

-- // Email

newtype EmailAddress = EmailAddress Text deriving (Show, Eq, Generic, FromJSON, ToJSON)
instance IsString EmailAddress where
  fromString = EmailAddress . T.pack
instance ToField EmailAddress where
  toField (EmailAddress e) = toField e
instance FromField EmailAddress where
  fromField f mbs = EmailAddress <$> fromField f mbs
instance FromHttpApiData EmailAddress where
  parseUrlPiece t = EmailAddress <$> parseUrlPiece t
instance ToHttpApiData EmailAddress where
  toUrlPiece (EmailAddress addr) = addr
