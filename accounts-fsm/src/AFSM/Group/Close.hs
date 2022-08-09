module AFSM.Group.Close where

import           AFSM.FSM.Group                as FSM
                                                ( GroupEvent
                                                , closeGroup
                                                , groupStateChangeEvents
                                                , increaseGroupRevision
                                                , verifyState
                                                )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB(..) )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( GroupId
                                                , UserID
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

closeGroup
  :: (HasGetGroupDB m, HasSaveGroupDB m, MonadIO m)
  => TraceContext
  -> GroupId
  -> UserID
  -> m [GroupEvent]
closeGroup trace groupId closer = do
  let mid = traceToMID trace
  tracePrint trace "CloseGroup " (groupId, closer)

  groupModel <- fromJust <$> getGroupByGroupId groupId

  let (evts, newModel) =
        ([], groupModel)
          & FSM.closeGroup closer
          & verifyState
          & groupStateChangeEvents closer groupModel
          & increaseGroupRevision mid

  _ <- saveGroupModel newModel
  return evts
