{-# LANGUAGE StrictData, RecordWildCards #-}

{-|
Module      : FSM
Description : Finite state machine functions for user and group
Maintainer  : adam@example.com
Stability   : experimental
-}
{-# LANGUAGE NamedFieldPuns #-}
module AFSM.FSM.Group
  ( createGroup
  , changeRatio
  , setApproved
  , setAccepted
  , approveRatio
  , verifyState
  , groupStateChangeEvents
  , increaseGroupRevision
  , denyGroup
  , acceptMembership
  , blankGroup
  , closeGroup
  , GroupEvent(..)
  ) where

import           Data.Time                      ( UTCTime )
import           GHC.Stack                      ( HasCallStack )
import           Shared.Models.Group            ( GroupId
                                                , GroupMember(..)
                                                , GroupModel(..)
                                                , GroupSplit(..)
                                                , GroupStatus(..)
                                                , verifyGroupCohesion
                                                )
import           Shared.Models.User            as U
                                                ( UserID )
import           Shared.TgthrMessages.Base      ( MessageID )

type Creator = UserID
type AffectedUsers = [UserID]
type Changer = UserID
type Acceptor = UserID

data GroupEvent
  = EventGroupCreated GroupId Creator AffectedUsers
  | EventGroupSplitChanged GroupId Changer AffectedUsers
  | EventGroupStateChanged GroupId GroupStatus Changer AffectedUsers
  | EventGroupInviteAccepted GroupId Acceptor AffectedUsers
  deriving (Eq, Show)

createGroup
  :: HasCallStack
  => MessageID
  -> UTCTime
  -> GroupId
  -> Creator
  -> [UserID]
  -> ([GroupEvent], GroupModel)
createGroup mid now gid inviter members =
  let grp = verifyGroupCohesion $ GroupModel { grpId        = gid
                                             , grpStatus    = GroupCreated
                                             , grpStart     = Nothing
                                             , grpEnd       = Nothing
                                             , grpSplit     = equalSplit members
                                             , grpMembers   = groupMembers
                                             , grpRevision  = 1
                                             , grpVersion   = "1.0"
                                             , grpMsgSource = mid
                                             , grpCreatedAt = now
                                             }
  in  ([EventGroupCreated gid inviter (splUser <$> grpSplit grp)], grp)
 where
  createGroupMbr member | member == inviter = GroupMember member True
                        | otherwise         = GroupMember member False
  groupMembers = fmap createGroupMbr members
  equalSplit []     = error "Error: no members"
  equalSplit [a]    = [GroupSplit a 100 False]
  equalSplit [a, b] = [GroupSplit a 50 False, GroupSplit b 50 False]
  equalSplit [a, b, c] =
    [GroupSplit a 34 False, GroupSplit b 33 False, GroupSplit c 33 False]
  equalSplit [a, b, c, d] =
    [ GroupSplit a 25 False
    , GroupSplit b 25 False
    , GroupSplit c 25 False
    , GroupSplit d 25 False
    ]
  equalSplit _ = error "Error: group too large"

changeRatio
  :: MessageID
  -> UserID
  -> [GroupSplit]
  -> GroupModel
  -> ([GroupEvent], GroupModel)
changeRatio mid cscUser cscSplit model =
  let newModel = model { grpSplit = cscSplit, grpMsgSource = mid }
      output =
        ( [ EventGroupSplitChanged (grpId model)
                                   cscUser
                                   (splUser <$> grpSplit model)
          ]
        , newModel
        )
  in  approveRatio cscUser output

setApproved :: UserID -> GroupSplit -> GroupSplit
setApproved uid spl@GroupSplit {..} =
  if splUser == uid then spl { splApproved = True } else spl

setAccepted :: UserID -> GroupMember -> GroupMember
setAccepted uid mbrs@GroupMember {..} =
  if mbrUser == uid then mbrs { mbrAccepted = True } else mbrs

approveRatio
  :: UserID -> ([GroupEvent], GroupModel) -> ([GroupEvent], GroupModel)
approveRatio approvingUser (evts, model) =
  let newSplit    = fmap (setApproved approvingUser) (grpSplit model)
      newApproval = fmap (setAccepted approvingUser) (grpMembers model) -- if you approve then you acccept
      revised     = model { grpSplit = newSplit, grpMembers = newApproval }
  in  (evts, revised)

foldApprovals :: GroupMember -> Bool -> Bool
foldApprovals member accum = accum && mbrAccepted member

verifyState :: ([GroupEvent], GroupModel) -> ([GroupEvent], GroupModel)
verifyState (evs, currModel) = (evs, newModel)
 where
  newModel
    | -- If everyone has appoved membership and we are not already inactive, go active
      and
      [ foldr foldApprovals True (grpMembers currModel)
      , grpStatus currModel /= GroupExpired
      , grpStatus currModel /= GroupDenied
      , grpStatus currModel /= GroupClosed
      ]
    = currModel { grpStatus = GroupActive }
    | -- If we were just created, move immediatly without passing go to pending
      grpStatus currModel == GroupCreated
    = currModel { grpStatus = GroupPending }
    | -- Otherwise keep current state
      otherwise
    = currModel

groupStateChangeEvents
  :: UserID
  -> GroupModel
  -> ([GroupEvent], GroupModel)
  -> ([GroupEvent], GroupModel)
groupStateChangeEvents changer GroupModel { grpStatus = oldStatus } (evts, newModel@GroupModel { grpStatus = newStatus, grpId })
  = (evts <> newEvts, newModel)
 where
  newEvts =
    [ EventGroupStateChanged grpId
                             newStatus
                             changer
                             (mbrUser <$> grpMembers newModel)
    | oldStatus /= newStatus
    ]

increaseGroupRevision
  :: MessageID -> ([GroupEvent], GroupModel) -> ([GroupEvent], GroupModel)
increaseGroupRevision mid (es, m) =
  (es, m { grpRevision = grpRevision m + 1, grpMsgSource = mid })

denyGroup :: UserID -> GroupModel -> ([GroupEvent], GroupModel)
denyGroup uidDenier model@GroupModel {..} =
  ( [EventGroupStateChanged grpId grpStatus uidDenier users]
  , model { grpStatus = GroupDenied }
  )
  where users = fmap splUser grpSplit

acceptMembership
  :: UserID -> ([GroupEvent], GroupModel) -> ([GroupEvent], GroupModel)
acceptMembership acceptUid (evts, model@GroupModel {..}) =
  ( EventGroupInviteAccepted grpId acceptUid (mbrUser <$> grpMembers) : evts
  , model { grpMembers = fmap (activate acceptUid) grpMembers }
  )
 where
  activate user g@GroupMember {..} =
    if user == mbrUser then g { mbrAccepted = True } else g

blankGroup :: MessageID -> GroupModel -> ([GroupEvent], GroupModel)
blankGroup mid group = ([], group { grpMsgSource = mid })

closeGroup
  :: UserID -> ([GroupEvent], GroupModel) -> ([GroupEvent], GroupModel)
closeGroup uidCloser (evts, model@GroupModel {..}) =
  ( EventGroupStateChanged grpId GroupClosed uidCloser users : evts
  , model { grpStatus = GroupClosed }
  )
  where users = fmap splUser grpSplit
