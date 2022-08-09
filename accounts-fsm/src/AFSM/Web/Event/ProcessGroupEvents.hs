{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module AFSM.Web.Event.ProcessGroupEvents where

import           AFSM.AppMonad                  ( CanProcessGroupEvents )
import           AFSM.FSM.Group                 ( GroupEvent(..) )
import           AFSM.Group.Close               ( closeGroup )
import           AFSM.Group.Query.ByGroupState  ( getAliveGroups )
import           AFSM.IO.Random                 ( HasRandom(getUUID) )
import           AFSM.IO.Time                   ( GetCurrentTime(getCurrentTime)
                                                )
import           AFSM.Monad.HasEventTracking    ( HasEventTracking(..) )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB )
import           AFSM.Web.Admin.SyncSegment     ( syncGroupState )
import           AFSM.Web.Rewards.Rewards       ( activateRewardForGroup )
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , fromMaybe
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , diffUTCTime
                                                , nominalDay
                                                )
import           Data.UUID                      ( fromText )
import           Shared.Models.Group            ( GroupId
                                                , GroupMember(..)
                                                , GroupModel(..)
                                                , GroupSplit(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.Ids              ( ActivatedRewardId(..)
                                                , RewardId(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.AccountsFSM.API  ( ActivateRewardBody(..)
                                                , TraceContext
                                                )

processGroupEvent
  :: (CanProcessGroupEvents m) => TraceContext -> GroupEvent -> m ()
processGroupEvent _ (EventGroupCreated groupId creator affected) = do
  inviterMaybe <- getUserById creator
  let sendInvite inviter userId = do
        thisUser <- getUserById userId
        case thisUser of
          Nothing      -> return ()
          Just invitee -> trackEventWithProps userId "User Invited" $ object
            [ "inviterId" .= creator
            , "inviterEmail" .= usrEmail inviter
            , "inviterFName" .= fromMaybe "" (usrFirstName inviter)
            , "inviterLName" .= fromMaybe "" (usrLastName inviter)
            , "inviteeEmail" .= usrEmail invitee
            , "inviteeFName" .= fromMaybe "Partner" (usrFirstName invitee)
            , "groupId" .= groupId
            ]
  case inviterMaybe of
    Nothing      -> return ()
    -- don't invite the crator
    Just inviter -> mapM_ (sendInvite inviter) . filter (/= creator) $ affected

  return ()
processGroupEvent _ (EventGroupSplitChanged gid _ affected) = mapM_
  (\userId -> do
    groupM <- getGroupByGroupId gid
    case groupM of
      Nothing                               -> return ()
      Just GroupModel { grpSplit = splits } -> do
        let floatsplits :: [Double] = fromRational . splRatio <$> splits
        let approvals :: [Bool]     = splApproved <$> splits

        trackEventWithProps userId "Group Split Changed" $ object
          [ "splits" .= floatsplits
          , "approvals" .= approvals
          , "allApproved" .= and approvals
          ]
  )
  affected
processGroupEvent trace (EventGroupStateChanged groupId GroupActive changer affected)
  = do
    mapM_ (\userId -> trackEventWithProps userId "Group Active" $ object [])
          affected

    evts <- concat <$> mapM (closeOtherGroups trace changer groupId) affected

    -- If both users have become active in the last 24 hours then we need to assing them the special boost
    now <- getCurrentTime
    activeWithin24Hours <-
      filter (activeWithin24Hrs now) . catMaybes <$> mapM getUserById affected
    if length activeWithin24Hours /= length affected
      then return ()
      else do
        let quickSignupBoost =
              RewardId
                . fromJust
                . fromText
                $ "00000000-0000-0000-0000-000000000000"
        newActivationId <- ActivatedRewardId <$> getUUID
        _               <- activateRewardForGroup
          trace
          groupId
          ActivateRewardBody { activatedBy      = changer
                             , rewardToActivate = quickSignupBoost
                             , newActivationId  = newActivationId
                             }
        return ()

    mapM_ (processGroupEvent trace) evts
processGroupEvent _ (EventGroupStateChanged gid GroupClosed _ affected) = do
  mapM_
    (\userId -> do
      trackEventWithProps userId "Group Closed" $ object []
      trackOneOffTrait userId ["groupStatus" .= GroupClosed]
    )
    affected

  group <- getGroupByGroupId gid
  case group of
    Nothing -> return ()
    Just GroupModel { grpMembers = members } ->
      mapM_ syncGroupState $ fmap mbrUser members

  return ()
processGroupEvent _ (EventGroupStateChanged gid groupStatus _ affected) = do
  mapM_ (\userId -> trackOneOffTrait userId ["groupStatus" .= groupStatus])
        affected

  group <- getGroupByGroupId gid
  case group of
    Nothing -> return ()
    Just GroupModel { grpMembers = members } ->
      mapM_ syncGroupState $ fmap mbrUser members

  return ()
processGroupEvent _ EventGroupInviteAccepted{} = return ()

closeOtherGroups
  :: ( HasGetGroupDB m
     , HasSaveGroupDB m
     , GetCurrentTime m
     , MonadIO m
     , MonadMask m
     )
  => TraceContext
  -> UserID
  -> GroupId
  -> UserID
  -> m [GroupEvent]
closeOtherGroups trace closer groupToKeep userId = do
  currentGroups <- getAliveGroups userId
  let otherGroups =
        filter (\GroupModel {..} -> grpId /= groupToKeep) currentGroups
  evts <- mapM
    (\GroupModel {..} ->
      retryFn trace "closeGroup" $ closeGroup trace grpId closer
    )
    otherGroups
  return $ concat evts

activeWithin24Hrs :: UTCTime -> UserModel -> Bool
activeWithin24Hrs _ UserModel { usrBecameActiveOn = Nothing } = False
activeWithin24Hrs now UserModel { usrBecameActiveOn = Just t } =
  diffUTCTime now t < nominalDay
