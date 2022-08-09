module AFSM.User.Change.Password
  ( changePassword
  ) where

import           AFSM.FSM.User                  ( UserEvent
                                                , increaseUserRevision
                                                , updatePassword
                                                )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(getUserById) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust )
import           Shared.Models.User             ( Password
                                                , UserID
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

changePassword
  :: (HasGetUserDB m, HasSaveUserDB m, MonadIO m)
  => TraceContext
  -> UserID
  -> Password
  -> m [UserEvent]
changePassword trace uid pass = do
  let mid = traceToMID trace
  model <- getUserById uid <&> fromJust

  let (events, newUser) =
        model & updatePassword pass & increaseUserRevision mid

  -- print debug diff
  diffUser trace model newUser

  saveUserModel newUser

  return events
