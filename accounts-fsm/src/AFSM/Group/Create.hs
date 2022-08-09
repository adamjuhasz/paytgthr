{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module AFSM.Group.Create
  ( InvtingError(..)
  , createNewGroup
  , createActiveGroup
  , InviteInfo(..)
  ) where

import           AFSM.FSM.Group                 ( GroupEvent
                                                , acceptMembership
                                                , approveRatio
                                                , closeGroup
                                                , createGroup
                                                , groupStateChangeEvents
                                                , increaseGroupRevision
                                                , verifyState
                                                )
import           AFSM.FSM.User                  ( UserEvent )
import           AFSM.IO.Time                   ( GetCurrentTime(getCurrentTime)
                                                )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB )
import           AFSM.User.Create               ( createUser )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.List                      ( find
                                                , sort
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Group            ( GroupMember(..)
                                                , GroupModel(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.Ids              ( GroupId
                                                , UserID
                                                )
import           Shared.Models.User             ( EmailAddress
                                                , UserModel
                                                  ( usrEmail
                                                  , usrFirstName
                                                  , usrUserID
                                                  )
                                                , normalizeEmail
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data InvtingError
  = InviterMissing
  | InviterMissingFirstName
  | CantInviteYourself
  | GroupCountTooSmall
  deriving (Eq, Show)
instance Exception InvtingError

createNewGroup
  :: ( HasGetUserDB m
     , HasGetGroupDB m
     , HasSaveGroupDB m
     , MonadIO m
     , GetCurrentTime m
     , HasSaveUserDB m
     )
  => TraceContext
  -> GroupId
  -> UserID
  -> [(Text, EmailAddress, UserID)]
  -> m ([UserEvent], [GroupEvent], [InviteInfo], GroupId)
createNewGroup trace newGroupId inviter members = do
  let mid = traceToMID trace

  when (length members < 2) (throw GroupCountTooSmall)
  let nonEmptyMembers = NE.fromList members
  -- get or create partners
  let getPartner      = getOrCreatePartner trace newGroupId inviter
  partnerInfo <- mapM getPartner nonEmptyMembers
  let partnerNEIds = NE.sort $ fmap (\(uid, _, _) -> uid) partnerInfo
  let partnerEvts  = concatMap (\(_, es, _) -> es) partnerInfo
  let invites      = NE.toList $ fmap (\(_, _, is) -> is) partnerInfo
  currentGroups <- getGroupsForUserFiltered
    [GroupCreated, GroupPending, GroupActive, GroupPaused]
    (NE.head partnerNEIds)

  -- any ids doubled? 
  when (NE.length partnerNEIds /= NE.length (NE.nub partnerNEIds))
       (throw CantInviteYourself)

  -- Check if already have this group
  let partnerIds = sort $ NE.toList partnerNEIds
  let pullOutMembers :: GroupModel -> (GroupId, [UserID])
      pullOutMembers GroupModel {..} = (grpId, sort (mbrUser <$> grpMembers))
  let groupMembers     = fmap pullOutMembers currentGroups
  let groupAlreadyMade = find (\(_, mbrs) -> partnerIds == mbrs) groupMembers

  tracePrint trace
             "createGroup (partnerIds, groupMembers, groupAlreadyMade): "
             (partnerIds, groupMembers, groupAlreadyMade)

  case groupAlreadyMade of
    Just (gid, _) -> return ([], [], invites, gid)
    Nothing       -> do
      now <- getCurrentTime
      let (evts, model) = createGroup mid now newGroupId inviter partnerIds
      _ <- saveGroupModel model

      tracePrint trace
                 "createGroup createGroup: "
                 (partnerIds, evts, model, currentGroups)

      let groupEvents = evts
      return (partnerEvts, groupEvents, invites, grpId model)

data InviteInfo = InviteInfo
  { inviterUserId    :: UserID
  , invitedUserId    :: UserID
  , invitedEmail     :: EmailAddress
  , invitedFirstName :: Text
  , newGroupId       :: GroupId
  }
  deriving (Eq, Show)

getOrCreatePartner
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> GroupId
  -> UserID
  -> (Text, EmailAddress, UserID)
  -> m (UserID, [UserEvent], InviteInfo)
getOrCreatePartner trace gid inviter (fname, email, uuid) = do
  let mid       = traceToMID trace

  let normEmail = normalizeEmail email
  userMaybe <- getUserByEmail normEmail
  case userMaybe of
    Just user -> return
      ( usrUserID user
      , []
      , InviteInfo inviter
                   (usrUserID user)
                   (usrEmail user)
                   (fromMaybe "" (usrFirstName user))
                   gid
      )
    Nothing -> do
      tracePrint trace
                 "Creating a partner "
                 (inviter, (fname, normEmail, uuid, email))
      (uid, evts) <- createUser mid normEmail Nothing (Just fname) uuid
      return (uid, evts, InviteInfo inviter uid normEmail fname gid)

createActiveGroup
  :: ( HasGetUserDB m
     , HasGetGroupDB m
     , HasSaveGroupDB m
     , GetCurrentTime m
     , MonadIO m
     )
  => TraceContext
  -> (UserID, UserID)
  -> GroupId
  -> m [GroupEvent]
createActiveGroup trace (inviter, invitee) newGroupId = do
  let mid = traceToMID trace
  -- Each user can only have 1 primary group, close every partner's other groups
  inviterGroups <- getGroupsForUser inviter
  inviteeGroups <- getGroupsForUser invitee

  let closeGrp GroupModel { grpStatus = GroupClosed } = return []
      closeGrp aGroup = do
        let (events, model) =
              ([], aGroup) & closeGroup invitee & increaseGroupRevision mid
        saveGroupModel model
        return events

  closeEvents <- concat <$> mapM closeGrp (inviterGroups <> inviteeGroups)

  now         <- getCurrentTime
  let (createEvents, createModel) =
        createGroup mid now newGroupId inviter [inviter, invitee]
  let (acceptEvents, activeModel) =
        ([], createModel)
          & acceptMembership inviter
          & acceptMembership invitee
          & approveRatio inviter
          & approveRatio invitee
          & verifyState
          & groupStateChangeEvents invitee createModel
          & increaseGroupRevision mid

  saveGroupModel activeModel

  let events = closeEvents <> createEvents <> acceptEvents
  tracePrint trace
             "createInvitedGroup "
             ((inviter, invitee), newGroupId, activeModel, events)

  return events
