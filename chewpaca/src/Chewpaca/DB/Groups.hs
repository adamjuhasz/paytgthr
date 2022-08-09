{-# LANGUAGE QuasiQuotes #-}

module Chewpaca.DB.Groups where

import           Data.Maybe                     ( listToMaybe )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection
                                                , In(In)
                                                , Only(Only)
                                                , query
                                                , query_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Group            ( GroupId
                                                , GroupModel
                                                , groupModelFields
                                                )

getGroups :: Connection -> IO [GroupModel]
getGroups conn = query_
  conn
  (  "SELECT "
  <> fst groupModelFields
  <> " FROM tgthr.groups_current_rev ORDER BY created_at DESC"
  )


getGroupsForUser :: UUID -> Connection -> IO [(GroupModel, UTCTime)]
getGroupsForUser uid conn = do
  let params = (uid, uid)
      specifier = [sql|
            FROM tgthr.groups_current_rev
            WHERE (split -> 0 ->> 'user' = ? OR split -> 1 ->> 'user' = ?)
            ORDER BY created_at DESC
        |]
  groups :: [GroupModel] <- query
    conn
    ("SELECT " <> fst groupModelFields <> specifier)
    params
  times :: [UTCTime] <-
    fmap (\(Only x) -> x)
      <$> query conn ("SELECT created_at " <> specifier) params
  return $ zip groups times

getGroupById :: GroupId -> Connection -> IO [(GroupModel, UTCTime)]
getGroupById gid conn = do
  let specifier = " FROM tgthr.groups WHERE id = ? ORDER BY revision DESC"
  groups :: [GroupModel] <- query
    conn
    ("SELECT " <> fst groupModelFields <> specifier)
    (Only gid)
  times :: [UTCTime] <- fmap (\(Only x) -> x)
    <$> query conn ("SELECT created_at " <> specifier) (Only gid)
  return $ zip groups times

getJustGroupByIdAndRev :: (GroupId, Int) -> Connection -> IO (Maybe GroupModel)
getJustGroupByIdAndRev (gid, revision) conn = listToMaybe
  <$> query conn qs (gid, revision)
 where
  qs = "SELECT " <> fst groupModelFields <> [sql| 
       FROM tgthr.groups
       WHERE id = ? AND revision = ?
       |]

getGroupsById :: [GroupId] -> Connection -> IO [GroupModel]
getGroupsById gids conn = query
  conn
  (  "SELECT"
  <> fst groupModelFields
  <> [sql| FROM tgthr.groups_current_rev WHERE id IN ? |]
  )
  (Only $ In gids)
