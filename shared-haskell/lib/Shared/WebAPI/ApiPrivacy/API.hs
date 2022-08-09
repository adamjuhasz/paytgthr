{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- Needed for stack 17.10 for import           GHC.Generics                   ( Generic )

module Shared.WebAPI.ApiPrivacy.API
  ( TraceContext(..)
  , traceToMID
  , CardCreatedResponse(..)
  , CreateCardholderAction(..)
  , module Shared.WebAPI.ApiPrivacy.API
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Servant
import           Servant.API.Generic
import           Shared.Models.Card             ( CardDesign
                                                , CardMemo
                                                , CardPinEnc
                                                , CardStatus
                                                , PrivacyCardToken
                                                )
import           Shared.Models.Ids              ( CardId
                                                , UserID
                                                )
import           Shared.WebAPI.General.API      ( TraceContext(..)
                                                , TraceHeaders
                                                , traceToMID
                                                )
import           Shared.WebAPI.General.Issuer   ( CardCreatedResponse(..)
                                                , CreateCardholderAction(..)
                                                )

data CardPciInfo = CardPciInfo
  { pan          :: Text
  , expMonth     :: Text
  , expShortYear :: Text
  , cvv          :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON CardPciInfo where
instance ToJSON CardPciInfo where

data CardCreateBody = CardCreateBody
  { newCardId     :: CardId
  , newCardDesign :: CardDesign
  , newCardMemo   :: CardMemo
  }
  deriving (Eq, Show, Generic)
instance FromJSON CardCreateBody where
instance ToJSON CardCreateBody where

data ChangePinBody = ChangePinBody
  { newPinText      :: Text
  , newPinEncrypted :: CardPinEnc
  }
  deriving (Eq, Show, Generic)
instance FromJSON ChangePinBody where
instance ToJSON ChangePinBody where

-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  { _CardCreate       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> ReqBody '[JSON] CardCreateBody :>  Post '[JSON] CardCreatedResponse
  , _CardClose        :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Capture "PrivacyCardToken" PrivacyCardToken :> "action" :> "close" :> Post '[JSON] NoContent
  , _CardHostedUI     :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Capture "PrivacyCardToken" PrivacyCardToken :> "pciui" :> Get '[JSON] Text
  , _CardPCIInfo      :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Capture "PrivacyCardToken" PrivacyCardToken :> "pci" :> Get '[JSON] CardPciInfo
  , _CardChangeState  :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Capture "PrivacyCardToken" PrivacyCardToken :> Capture "CardStatus" CardStatus :> Put '[JSON] NoContent
  , _CardChangePin    :: route :- TraceHeaders :>  "user" :> Capture "userid" UserID :> "card" :> Capture "PrivacyCardToken" PrivacyCardToken :> "pin" :> ReqBody '[JSON] ChangePinBody :> Post '[JSON] NoContent

  -- cardholder
  , _CardholderCreate :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "cardholder" :>  Post '[JSON] CreateCardholderAction

  -- sandbox
  , _SandboxAuth      :: route :- TraceHeaders :> "sandbox" :> "simulate" :> "authorize" :> Capture "userid" UserID :> ReqBody '[JSON] Double :> Post '[JSON] Text
  , _SandboxVoid      :: route :- TraceHeaders :> "sandbox" :> "simulate" :> "void"      :> Capture "token" Text    :> ReqBody '[JSON] Double :> Post '[JSON] NoContent
  , _SandboxClear     :: route :- TraceHeaders :> "sandbox" :> "simulate" :> "clearing"  :> Capture "token" Text    :> ReqBody '[JSON] Double :> Post '[JSON] NoContent
  , _SandboxReturn    :: route :- TraceHeaders :> "sandbox" :> "simulate" :> "return"    :> Capture "userid" UserID :> ReqBody '[JSON] Double :> Post '[JSON] Text

  -- system
  , _health           :: route :- "_health" :> Get '[PlainText] Text
  }
  deriving Generic
