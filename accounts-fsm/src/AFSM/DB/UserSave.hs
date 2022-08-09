{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module AFSM.DB.UserSave
  ( saveUser
  , saveCard
  , saveAKYCAssesment
  , saveAUserNote
  , saveUserDeviceIP
  ) where

import           AFSM.Monad.HasSaveUserDB       ( IPAddress
                                                , IPCountry
                                                , Revision
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( getCurrentTime )
import           Database.PostgreSQL.Simple     ( Connection
                                                , execute
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Card             ( CardModel
                                                , cardSqlFields
                                                )
import           Shared.Models.KYCAssesment     ( KYCAssesment
                                                , kycAssesmentFields
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                , userModelFieldOrder
                                                , userModelPlaceHolders
                                                )

saveUser :: UserModel -> Connection -> IO ()
saveUser user conn = do
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

saveCard :: CardModel -> Connection -> IO ()
saveCard card conn = do
  _ <- execute conn insert card
  _ <- execute conn update card
  return ()
 where
  insert =
    "INSERT INTO tgthr.cards ( "
      <> fst cardSqlFields
      <> " ) VALUES ( "
      <> snd cardSqlFields
      <> " )"
  update =
    "UPSERT INTO tgthr.cards_current_rev ( "
      <> fst cardSqlFields
      <> " ) VALUES ( "
      <> snd cardSqlFields
      <> " )"

saveAKYCAssesment :: KYCAssesment -> Connection -> IO ()
saveAKYCAssesment assesment conn = do
  _ <- execute conn insert assesment
  return ()
 where
  insert =
    "INSERT INTO tgthr.kyc_assesments ( "
      <> fst kycAssesmentFields
      <> " ) VALUES ( "
      <> snd kycAssesmentFields
      <> " ) "

saveAUserNote :: UserID -> (Text, Revision) -> Connection -> IO ()
saveAUserNote uid (note, rev) conn = do
  now <- getCurrentTime
  _   <- execute conn insert (uid, rev, now, note)
  _   <- execute conn update (uid, rev, now, note)
  return ()
 where
  insert = [sql| 
      INSERT INTO tgthr.user_notes 
      (user_id, revision, updated_at, note) 
      VALUES 
      (?, ?, ?, ?) 
      |]
  update = [sql| 
      UPSERT INTO tgthr.user_notes_current_rev
      (user_id, revision, updated_at, note) 
      VALUES 
      (?, ?, ?, ?) 
      |]


saveUserDeviceIP :: UserID -> (IPAddress, IPCountry) -> Connection -> IO ()
saveUserDeviceIP uid (ip, country) conn = do
  now <- getCurrentTime
  _   <- execute conn update (uid, ip, country, now)
  return ()
 where
  update = [sql| 
      UPSERT INTO tgthr.user_device_ip 
      (user_id, ip_address, ip_country, last_seen) 
      VALUES 
      (?, ?, ?, ?) 
      |]
