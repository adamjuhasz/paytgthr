{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Shared.Models.Cardholder where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , withText
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Text.Read                      ( readEither )

newtype PrivacyAccountToken = PrivacyAccountToken Text deriving (Eq, Show, Read)
instance FromJSON PrivacyAccountToken where
  parseJSON = withText "PrivacyAccountToken" $ return . PrivacyAccountToken
instance ToJSON PrivacyAccountToken where
  toJSON (PrivacyAccountToken tok) = toJSON tok
instance ToHttpApiData PrivacyAccountToken where
  toUrlPiece (PrivacyAccountToken t) = t

data CardholderId
  = AptoPaymentsCH AptoCardholderId
  | PayWithPrivacyCH PrivacyAccountToken
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
instance ToHttpApiData CardholderId where
  toUrlPiece cid = T.pack $ show cid
instance FromHttpApiData CardholderId where
  parseUrlPiece t = case readEither (T.unpack t) of
    Left  e   -> Left $ T.pack e
    Right cid -> Right cid
