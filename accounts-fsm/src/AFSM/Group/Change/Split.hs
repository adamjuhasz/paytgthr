module AFSM.Group.Change.Split
  ( SplitErrors(..)
  , approveGroupSplit
  , changeGroupSplit
  , forceSplitChange
  , setCategorySplits
  ) where

import           AFSM.FSM.Group                 ( GroupEvent
                                                , approveRatio
                                                , changeRatio
                                                , groupStateChangeEvents
                                                , increaseGroupRevision
                                                , verifyState
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB(..) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( when )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Shared.Console
import           Shared.Models.CategorySplit    ( CategoryCode(..)
                                                , CategorySplit(..)
                                                , CategoryState(..)
                                                )
import           Shared.Models.Group            ( GroupId
                                                , GroupModel(grpStatus)
                                                , GroupSplit(..)
                                                , GroupStatus
                                                  ( GroupActive
                                                  , GroupClosed
                                                  )
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data SplitErrors
  = SplitIsNotHundred
  | SplitIsEmpty
  | CantDisable000
  deriving (Eq, Show)
instance Exception SplitErrors

verifySplit :: Monad m => [GroupSplit] -> m ()
verifySplit split = do
  let total = sum $ fmap splRatio split

  when (null split)   (throw SplitIsEmpty)
  when (total /= 100) (throw SplitIsNotHundred)

changeGroupSplit
  :: (HasGetGroupDB m, HasSaveGroupDB m, MonadIO m)
  => TraceContext
  -> GroupId
  -> UserID
  -> [GroupSplit]
  -> m [GroupEvent]
changeGroupSplit trace gid changer newSplit = do
  let mid = traceToMID trace

  tracePrint trace "ChangeGroupSplit " (gid, changer, newSplit)
  verifySplit newSplit

  group <- fromJust <$> getGroupByGroupId gid
  let (evts, newModel) =
        group
          & changeRatio mid changer newSplit
          & verifyState
          & groupStateChangeEvents changer group
          & increaseGroupRevision mid

  _ <- saveGroupModel newModel
  return evts

approveGroupSplit
  :: (HasGetGroupDB m, HasSaveGroupDB m, MonadIO m)
  => TraceContext
  -> GroupId
  -> UserID
  -> m [GroupEvent]
approveGroupSplit trace gid approver = do
  let mid = traceToMID trace

  tracePrint trace "ApproveGroupSplit " (gid, approver)

  group <- fromJust <$> getGroupByGroupId gid
  let (evts, newModel) =
        ([], group)
          & approveRatio approver
          & verifyState
          & groupStateChangeEvents approver group
          & increaseGroupRevision mid

  _ <- saveGroupModel newModel
  return evts

forceSplitChange
  :: (HasGetGroupDB m, HasSaveGroupDB m, MonadIO m)
  => TraceContext
  -> GroupId
  -> [GroupSplit]
  -> m [GroupEvent]
forceSplitChange trace gid newSplit = do
  let mid = traceToMID trace

  tracePrint trace "ForceSplitChange " (gid, newSplit)
  verifySplit newSplit

  group <- fromJust <$> getGroupByGroupId gid

  let groupActive = if grpStatus group == GroupClosed
        then group { grpStatus = GroupActive } -- reopen group
        else group

  let users = fmap splUser newSplit
  let (evts, newModel) =
        changeRatio mid (head users) newSplit groupActive
          & (\x -> foldr approveRatio x users)
          & verifyState
          & groupStateChangeEvents (head users) group
          & increaseGroupRevision mid

  _ <- saveGroupModel newModel
  return evts

setCategorySplits
  :: (HasGetGroupDB m, HasSaveGroupDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> GroupId
  -> CategoryCode
  -> [GroupSplit]
  -> CategoryState
  -> m [GroupEvent]
setCategorySplits trace gid code newSplits newState = do
  now           <- getCurrentTime
  currentSplits <- getGroupSplits gid

  -- Verify states
  verifySplit newSplits
  when (code == Category000 && newState == CategoryDisabled)
       (throw CantDisable000)

  let currentSplitM = lookupSplit code currentSplits
  let currentSplit = fromMaybe
        (CategorySplit { groupId     = gid
                       , categoryId  = code
                       , catRevision = 0 -- becomes 1 lower
                       , createdAt   = now
                       , updatedAt   = now
                       , splits      = newSplits
                       , state       = newState
                       }
        )
        currentSplitM

  let newSplit = currentSplit { splits      = newSplits
                              , state       = newState
                              , catRevision = catRevision currentSplit + 1
                              , updatedAt   = now
                              }

  tracePrint trace
             "Changing split for group "
             (gid, code, newSplit, currentSplitM)

  case code of
    Category000 -> do
      -- will force saveCategorySplit
      let changer = head $ fmap splUser newSplits
      changeGroupSplit trace gid changer newSplits
    _ -> do
      saveCategorySplit newSplit
      return []

lookupSplit :: CategoryCode -> [CategorySplit] -> Maybe CategorySplit
lookupSplit _ [] = Nothing
lookupSplit code (cat@CategorySplit { categoryId = catCode } : cats) =
  if code == catCode then Just cat else lookupSplit code cats
