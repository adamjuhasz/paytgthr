{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AFSM.DB.GroupGet
  ( getGroupById
  , getGroupsByUserId
  , getGroupsByUserIdFiltered
  , getGroupDefaultSplit
  , getGroupCatSplits
  ) where

import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.PostgreSQL.Simple     ( Connection
                                                , In(In)
                                                , Only(..)
                                                , query
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(EscapeIdentifier)
                                                , ToField(..)
                                                )
import           Shared.Models.CategorySplit    ( CategorySplit
                                                , categorySplitFields
                                                )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupModel
                                                , GroupStatus
                                                , groupModelFields
                                                )
import           Shared.Models.User             ( UserID(..) )

getGroupById :: GroupId -> Connection -> IO (Maybe GroupModel)
getGroupById (GroupId gid) conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> fst groupModelFields
      <> " FROM tgthr.groups_current_rev WHERE id = ?"
  selector = Only gid

newtype JSONText = JSONText Text deriving (Eq, Show)
instance ToField JSONText where
  toField (JSONText t) = EscapeIdentifier . encodeUtf8 $ t

getGroupsByUserIdFiltered
  :: [GroupStatus] -> UserID -> Connection -> IO [GroupModel]
getGroupsByUserIdFiltered allowedStates (UserID uid) conn = query conn
                                                                  qs
                                                                  selector
 where
  qs = "SELECT " <> fst groupModelFields <> [sql| 
            FROM tgthr.groups_current_rev
            WHERE status IN ? AND (split -> 0 ->> 'user' = ? OR split -> 1 ->> 'user' = ?)
            ORDER BY CASE status WHEN 'active' THEN 1 WHEN 'paused' THEN 2 WHEN 'pending' THEN 3 ELSE 100 END ASC, created_at DESC
        |]
  selector = (In allowedStates, uid, uid)

getGroupsByUserId :: UserID -> Connection -> IO [GroupModel]
getGroupsByUserId (UserID uid) conn = query conn qs selector
 where
  qs = "SELECT " <> fst groupModelFields <> [sql| 
            FROM tgthr.groups_current_rev
            WHERE (split -> 0 ->> 'user' = ? OR split -> 1 ->> 'user' = ?)
            ORDER BY CASE status WHEN 'active' THEN 1 WHEN 'paused' THEN 2 WHEN 'pending' THEN 3 ELSE 100 END ASC, created_at DESC
        |]
  selector = (uid, uid)

getGroupDefaultSplit :: GroupId -> Connection -> IO (Maybe CategorySplit)
getGroupDefaultSplit gid conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> fst categorySplitFields
      <> [sql| FROM group_category_splits_current_rev WHERE group_id = ? AND category_id = 'Category000' |]
  selector = Only gid

getGroupCatSplits :: GroupId -> Connection -> IO [CategorySplit]
getGroupCatSplits gid conn = query conn qs selector
 where
  qs =
    "SELECT "
      <> fst categorySplitFields
      <> [sql| FROM group_category_splits_current_rev WHERE group_id = ? |]
  selector = Only gid
