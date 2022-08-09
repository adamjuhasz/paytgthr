{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AFSM.Web.Group.Change.CreateGroup where

import           AFSM.AppMonad                  ( CanProcessGroupEvents
                                                , CanProcessUserEvents
                                                )
import qualified AFSM.Group.Create             as AFSM
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Web.Event.ProcessGroupEvents
                                                ( processGroupEvent )
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Control.Monad.Catch            ( MonadCatch(catch) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Servant                        ( ServerError
                                                , err403
                                                )
import           Shared.Console                 ( traceError )
import           Shared.Models.Group            ( GroupId
                                                , GroupModel
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( CreateGroupBody(..)
                                                , TraceContext
                                                )

createGroup
  :: (CanProcessGroupEvents m, CanProcessUserEvents m, MonadError ServerError m)
  => TraceContext
  -> GroupId
  -> CreateGroupBody
  -> m GroupModel
createGroup trace groupId CreateGroupBody {..} = do
  res <-
    (Right <$> AFSM.createNewGroup trace groupId inviter members)
      `catch` (\(e :: AFSM.InvtingError) -> return $ Left e)
  case res of
    Left e -> do
      traceError trace "Error createGroup " (groupId, inviter, members, e)
      throwError err403
    Right (userEvents, groupEvents, invites, actualGrpId) -> do
      mapM_ (processUserEvent trace)  userEvents
      mapM_ (processGroupEvent trace) groupEvents
      mapM_ processInvite             invites

      groupM <- getGroupByGroupId actualGrpId
      case groupM of
        Just g  -> return g
        Nothing -> error $ "Error: createGroup, Can not find group " <> show
          (groupId, actualGrpId)

processInvite :: (Monad m) => AFSM.InviteInfo -> m ()
processInvite _ = return ()
