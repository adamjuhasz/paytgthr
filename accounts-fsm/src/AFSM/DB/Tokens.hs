{-# LANGUAGE QuasiQuotes #-}

module AFSM.DB.Tokens where

import           Data.Int                       ( Int64 )
import           Database.PostgreSQL.Simple     ( execute
                                                , query
                                                , Connection
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Token            ( UserToken
                                                , userTokenFields
                                                , TokenText
                                                )
import           Shared.Models.User             ( UserID(..) )

class Monad m => HasTokenDB m where
  getUserTokenWithCode :: TokenText -> UserID -> m [UserToken]
  saveUserToken ::UserToken -> m ()

findToken :: TokenText -> UserID -> Connection -> IO [UserToken]
findToken token (UserID uid) conn = query conn qs (uid, token)
 where
  qs =
    [sql| SELECT |]
      <> fst userTokenFields
      <> [sql| FROM tgthr.tokens WHERE user_id = ? AND token_code = ? |]

saveToken :: UserToken -> Connection -> IO Int64
saveToken token conn = execute conn qs token
 where
  qs =
    [sql| INSERT INTO tgthr.tokens ( |]
      <> fst userTokenFields
      <> [sql| ) VALUES ( |]
      <> snd userTokenFields
      <> [sql| ) |]
