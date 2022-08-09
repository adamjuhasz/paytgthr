module APIApto.Monad.Accounts where

import           Shared.Models.Group            ( GroupModel )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( ChangeKYCStateBody
                                                , TraceContext
                                                )

class Monad m => HasAccounts m where
  getUser :: TraceContext -> UserID -> m (Maybe UserModel)
  getGroupForUser :: TraceContext -> UserID -> m (Maybe GroupModel)
  setKycState :: TraceContext -> UserID -> ChangeKYCStateBody -> m ()
