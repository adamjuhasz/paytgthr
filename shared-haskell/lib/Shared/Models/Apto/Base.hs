{-# LANGUAGE DeriveGeneric, DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE StrictData #-}

module Shared.Models.Apto.Base where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , ToJSON
                                                , withObject
                                                )
import           Data.Aeson.Types               ( (<?>)
                                                , JSONPathElement(Key)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )

newtype AptoCardholderId = AptoCardholderId Text deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
instance FromHttpApiData AptoCardholderId where
  parseUrlPiece t = AptoCardholderId <$> parseUrlPiece t
instance ToHttpApiData AptoCardholderId where
  toUrlPiece = fromCardholder

fromCardholder :: AptoCardholderId -> Text
fromCardholder (AptoCardholderId t) = t

data AptoErrorResponse = AptoErrorResponse
  { aperType :: Text
  , aperMsg  :: Text
  }
  deriving (Eq, Show)
instance FromJSON AptoErrorResponse where
  parseJSON = withObject "AptoErrorResponse" $ \o -> do
    e        <- o .: "error"
    aperType <- e .: "type" <?> Key "error"
    aperMsg  <- e .: "message" <?> Key "error"

    return $ AptoErrorResponse { .. }
