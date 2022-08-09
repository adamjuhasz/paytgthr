{-# LANGUAGE QuasiQuotes #-}

module Chewpaca.DB.Users where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection
                                                , In(In)
                                                , Only(..)
                                                , Query
                                                , execute
                                                , query
                                                , query_
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                , userModelFieldOrder
                                                , userModelPlaceHolders
                                                )

getUsers :: Connection -> IO [UserModel]
getUsers conn = query_
  conn
  (  "SELECT "
  <> userModelFieldOrder
  <> " FROM tgthr.users_current_rev ORDER BY created_on DESC, id ASC"
  )

getUsersWithIds :: [UserID] -> Connection -> IO [UserModel]
getUsersWithIds ids conn = query
  conn
  (  "SELECT "
  <> userModelFieldOrder
  <> " FROM tgthr.users_current_rev WHERE id IN ?"
  )
  (Only $ In ids)

getJustUser :: UserID -> Connection -> IO UserModel
getJustUser uID conn = head <$> query conn qs (Only uID)
 where
  qs =
    "SELECT "
      <> userModelFieldOrder
      <> " FROM tgthr.users_current_rev WHERE id = ?"

getUser :: UUID -> Connection -> IO [(UserModel, UTCTime)]
getUser uID conn = do
  user  <- query conn ("SELECT " <> userModelFieldOrder <> selector) (Only uID)
  times <- fmap fromOnly
    <$> query conn ("SELECT created_at " <> selector) (Only uID)
  return $ zip user times
  where selector = " FROM tgthr.users WHERE id = ? ORDER BY revision DESC"

saveUser :: UserModel -> Connection -> IO ()
saveUser user conn = withTransaction conn $ do
  _ <- execute conn insert user
  _ <- execute conn update user
  return ()

 where
  insert =
    "INSERT INTO tgthr.users ( "
      <> userModelFieldOrder
      <> ") VALUES ( "
      <> userModelPlaceHolders
      <> ")"
  update =
    "UPSERT INTO tgthr.users_current_rev ( "
      <> userModelFieldOrder
      <> ") VALUES ( "
      <> userModelPlaceHolders
      <> ")"

searchUser :: Text -> Connection -> IO [UserModel]
searchUser queryTxt conn = query
  conn
  qs
  ( normalizedQuery
  , normalizedQuery
  , normalizedQuery
  , normalizedQuery
  , normalizedQuery
  , normalizedQuery
  , normalizedQuery
  , normalizedQuery
  )
 where
  normalizedQuery = "%" <> T.strip queryTxt <> "%"
  qs = "SELECT " <> userModelFieldOrder <> [sql| 
          FROM tgthr.users_current_rev 
          WHERE 
          (  email ILIKE ? 
          OR name_first || ' ' || name_last ILIKE ? 
          OR apto_cardholderid ILIKE ? 
          OR apto_cardid ILIKE ? 
          OR phone_number ILIKE ?
          OR address_street || ' ' || address_street2 || ' ' || address_city || ' ' || address_state || ' ' || address_zip ILIKE ? 
          OR bank_nickname ILIKE ?
          OR bank_name  ILIKE ?
          ) 
          ORDER BY created_on DESC
        |]

searchUsersAddress :: Text -> Connection -> IO [UserModel]
searchUsersAddress queryTxt conn = query conn qs (Only normalizedQuery)
 where
  normalizedQuery = "%" <> T.strip queryTxt <> "%"
  qs = "SELECT " <> userModelFieldOrder <> [sql| 
          FROM tgthr.users_current_rev 
          WHERE 
          (address_street || ' ' || address_street2 || ' ' || address_city || ' ' || address_state || ' ' || address_zip ILIKE ? 
          ) 
          ORDER BY created_on DESC
        |]

searchUsersBanking :: Text -> Connection -> IO [UserModel]
searchUsersBanking queryTxt conn = query
  conn
  qs
  (normalizedQuery, normalizedQuery, normalizedQuery, normalizedQuery)
 where
  normalizedQuery = "%" <> T.strip queryTxt <> "%"
  qs = "SELECT " <> userModelFieldOrder <> [sql| 
          FROM tgthr.users_current_rev 
          WHERE 
          (  bank_nickname ILIKE ?
          OR bank_name ILIKE ?
          OR bank_routing ILIKE ?
          OR bank_account ILIKE ?
          ) 
          ORDER BY created_on DESC
        |]

getUserNote :: UserID -> Connection -> IO Text
getUserNote uid conn = do
  res <- query
    conn
    [sql| 
      SELECT note
      FROM tgthr.user_notes_current_rev 
      WHERE user_id = ? 
    |]
    (Only uid)
  case res of
    []         -> return ""
    Only t : _ -> return t

getUsersInLast :: Query -> Connection -> IO [UserModel]
getUsersInLast interval conn = query_ conn qs
 where
  qs =
    "SELECT "
      <> userModelFieldOrder
      <> " FROM tgthr.users_current_rev WHERE created_on >= now() - interval '"
      <> interval
      <> "' ORDER BY created_on DESC"

getActiveUsers :: Connection -> IO [UserID]
getActiveUsers conn = fmap fromOnly <$> query_ conn qs
 where
  qs = [sql| SELECT id from tgthr.users_current_rev WHERE status = 'active' |]
