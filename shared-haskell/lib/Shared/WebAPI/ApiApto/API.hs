{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- Needed for stack 17.10 for import           GHC.Generics                   ( Generic )

module Shared.WebAPI.ApiApto.API
  ( Routes(..)
  , TraceContext(..)
  , traceToMID
  , CardCreatedResponse(..)
  , CreateCardholderAction(..)
  , CardActivateBody(..)
  , CardPinChangeBody(..)
  ) where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Servant
import           Servant.API.Generic
import           Shared.Models.Card             ( CardLastFour
                                                , IssuerPlatform
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..)
                                                , TraceHeaders
                                                , traceToMID
                                                )
import           Shared.WebAPI.General.Issuer   ( CardActivateBody(..)
                                                , CardCreatedResponse(..)
                                                , CardPinChangeBody(..)
                                                , CreateCardholderAction(..)
                                                )

-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  -- /user/:uid/card/
  { _CardActivate     :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> "action" :> "activate" :> ReqBody '[JSON] CardActivateBody :> Put '[JSON] NoContent
  , _CardChangePin    :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> "pin" :> ReqBody '[JSON]  CardPinChangeBody :> Put '[JSON] IssuerPlatform
  , _CardLastFour     :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> "lastfour" :> Get '[JSON] CardLastFour
  , _CardClose        :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> "action" :> "close" :> Post '[JSON] NoContent
  , _CardCreate       :: route :- TraceHeaders :> "user" :>  Capture "userid" UserID :> "card" :> Post '[JSON] CardCreatedResponse

  -- /user/:uid/cardholder/
  , _CardholderCreate :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "cardholder" :>  Post '[JSON] CreateCardholderAction
  , _CardholderUpdate :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "cardholder" :>  Put '[JSON] CreateCardholderAction

  -- /admin
  , _AptoAdminPull    :: route :- TraceHeaders :> "admin" :> "user" :> Capture "userid" UserID :>  "sync" :> "pull" :> Post '[JSON] NoContent

  -- system
  , _health           :: route :- "_health" :> Get '[PlainText] Text
  }
  deriving Generic
