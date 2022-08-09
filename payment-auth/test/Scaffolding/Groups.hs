module Scaffolding.Groups where

import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( nil )
import           Data.UUID.V4                   ( nextRandom )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupMember
                                                  ( GroupMember
                                                  , mbrAccepted
                                                  , mbrUser
                                                  )
                                                , GroupModel(..)
                                                , GroupSplit
                                                  ( GroupSplit
                                                  , splApproved
                                                  , splRatio
                                                  , splUser
                                                  )
                                                , GroupStatus(GroupActive)
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           System.IO.Unsafe               ( unsafePerformIO )

basicGroup :: GroupModel
basicGroup = GroupModel
  { grpId        = unsafePerformIO (GroupId <$> nextRandom)
  , grpStatus    = GroupActive
  , grpStart     = Nothing
  , grpEnd       = Nothing
  , grpSplit     = [ GroupSplit { splUser     = usrUserID userJohn
                                , splRatio    = 50
                                , splApproved = True
                                }
                   , GroupSplit { splUser     = usrUserID userJane
                                , splRatio    = 50
                                , splApproved = True
                                }
                   ]
  , grpMembers   = [ GroupMember { mbrUser     = usrUserID userJohn
                                 , mbrAccepted = True
                                 }
                   , GroupMember { mbrUser     = usrUserID userJane
                                 , mbrAccepted = True
                                 }
                   ]
  , grpRevision  = 1
  , grpVersion   = "1.0"
  , grpMsgSource = MessageID nil
  , grpCreatedAt = unsafePerformIO getCurrentTime
  }
