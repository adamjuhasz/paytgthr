{-# LANGUAGE RecordWildCards #-}

module AFSM.Web.Group.Change.GroupSplit where

import           AFSM.AppMonad                  ( CanProcessGroupEvents )
import qualified AFSM.Group.Change.Split       as AFSM
import           AFSM.Web.Event.ProcessGroupEvents
                                                ( processGroupEvent )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Ids              ( GroupId )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.AccountsFSM.API  ( ChangeSplitBody(..)
                                                , SetCatSplit(..)
                                                , TraceContext
                                                )

changeSplit
  :: (CanProcessGroupEvents m)
  => TraceContext
  -> GroupId
  -> ChangeSplitBody
  -> m NoContent
changeSplit trace groupId ChangeSplitBody {..} = do
  events <- retryFn trace "AFSM.changeGroupSplit"
    $ AFSM.changeGroupSplit trace groupId changer newSplits

  mapM_ (processGroupEvent trace) events

  return NoContent

setCatSplit
  :: (CanProcessGroupEvents m)
  => TraceContext
  -> GroupId
  -> [SetCatSplit]
  -> m NoContent
setCatSplit trace gid newSplits = do
  events <- retryFn trace "AFSM.setCategorySplits" $ mapM
    (\SetCatSplit {..} ->
      AFSM.setCategorySplits trace gid category split enabled
    )
    newSplits

  mapM_ (processGroupEvent trace) $ mconcat events

  return NoContent
