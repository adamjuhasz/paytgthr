module APIPrivacy.Monad.HasAccounts
  ( module APIPrivacy.Monad.HasAccounts
  , TraceContext(..)
  ) where

import           Shared.Models.Card             ( CardModel )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                )
import           Shared.WebAPI.General.API      ( TraceContext(..) )

class Monad m => HasAccounts m where
  getUser :: TraceContext -> UserID -> m (Maybe UserModel)
  getCardsFor :: TraceContext -> UserID -> m [CardModel]
