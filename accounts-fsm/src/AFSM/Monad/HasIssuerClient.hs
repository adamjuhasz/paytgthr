module AFSM.Monad.HasIssuerClient
  ( module AFSM.Monad.HasIssuerClient
  , module Shared.WebAPI.General.Issuer
  , module Shared.WebAPI.ApiPrivacy.API
  , TraceContext(..)
  , incrementTrace
  ) where

import           Shared.Models.Card             ( CardModel )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.ApiPrivacy.API   ( CardCreateBody(..)
                                                , ChangePinBody(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext(..)
                                                , incrementTrace
                                                )
import           Shared.WebAPI.General.Issuer   ( CardActivateBody(..)
                                                , CardCreatedResponse(..)
                                                , CreateCardholderAction(..)
                                                )

class Monad m => HasIssuerClient m where
  activateCard :: TraceContext -> UserID -> CardActivateBody -> m ()
  createCardholder ::  TraceContext -> UserID -> m CreateCardholderAction
  updateCardholder :: TraceContext -> UserID -> m CreateCardholderAction
  createCard :: TraceContext -> UserID -> CardCreateBody -> m CardCreatedResponse
  changeCardState :: TraceContext -> CardModel -> m ()
  setCardPin :: TraceContext -> CardModel -> ChangePinBody -> m ()
  pingIssuerService :: m ()
