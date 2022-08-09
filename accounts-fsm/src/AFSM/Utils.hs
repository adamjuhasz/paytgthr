{-# LANGUAGE StrictData, RecordWildCards #-}
module AFSM.Utils
  ( module AFSM.Utils
  , module Shared.Groups.SortGroups
  ) where

import           Shared.Groups.SortGroups       ( sortByTime
                                                , sortGroups
                                                )
import           Shared.Models.Group            ( GroupMember(..)
                                                , GroupSplit(..)
                                                )
import           Shared.Models.Ids              ( UserID )

hasUserApproved :: UserID -> [GroupSplit] -> Bool
hasUserApproved usrUserID = foldr userDidApprove False
 where
  userDidApprove GroupSplit {..} accum =
    (splUser == usrUserID) && splApproved || accum

hasUserAccepted :: UserID -> [GroupMember] -> Bool
hasUserAccepted usrUserID = foldr userDidAccept False
 where
  userDidAccept GroupMember {..} accum =
    (mbrUser == usrUserID) && mbrAccepted || accum
