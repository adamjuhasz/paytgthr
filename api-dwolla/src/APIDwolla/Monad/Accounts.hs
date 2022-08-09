module APIDwolla.Monad.Accounts where

import           Data.Text                      ( Text )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                , UserTrait
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasAccounts m where
  getUser    :: TraceContext -> UserID -> m (Maybe UserModel)
  updateUser :: TraceContext -> UserID -> [(UserTrait, Maybe Text)] -> m ()
