{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts #-}

module Shared.WebAPI.General.Issuer where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Card             ( CardDesign
                                                , CardLastFour
                                                , CardMemo
                                                , CardPinEnc
                                                , CardStatus
                                                , IssuerPlatform
                                                )
import           Shared.Models.Cardholder       ( CardholderId )

data CreateCardholderAction
  = CardholderCreated CardholderId
  | CardholderUpdated CardholderId
  | CardholderNotUpdated
  deriving (Eq, Show, Generic)
instance ToJSON CreateCardholderAction
instance FromJSON CreateCardholderAction

data CardCreatedResponse = CardCreatedResponse
  { createdCardId     :: IssuerPlatform
  , createdCardDesign :: CardDesign
  , createdCardMemo   :: CardMemo
  , createdStatus     :: CardStatus
  , createdLastFour   :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON CardCreatedResponse
instance FromJSON CardCreatedResponse

newtype CardPinChangeBody = CardPinChangeBody
  { encPin :: CardPinEnc
  }
  deriving (Eq, Show, Generic)
instance ToJSON CardPinChangeBody
instance FromJSON CardPinChangeBody

newtype CardActivateBody = CardActivateBody
  { cardLastFour :: CardLastFour
  }
  deriving (Eq, Show, Generic)
instance ToJSON CardActivateBody
instance FromJSON CardActivateBody
