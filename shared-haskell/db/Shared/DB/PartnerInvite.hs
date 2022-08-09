{-# LANGUAGE QuasiQuotes #-}

module Shared.DB.PartnerInvite where

import           Data.Maybe                     ( listToMaybe )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(Only)
                                                , execute
                                                , query
                                                , query_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Invite           ( InviteCode
                                                , InviteStatus(Created)
                                                , PartnerInvite
                                                , inviteFields
                                                )

saveAnInvite :: PartnerInvite -> Connection -> IO ()
saveAnInvite invite conn = do
  _ <- execute conn upsert invite
  return ()
 where
  upsert =
    "UPSERT INTO tgthr.partner_invites ( "
      <> fst inviteFields
      <> ") VALUES ( "
      <> snd inviteFields
      <> ")"

getInviteFromCode :: InviteCode -> Connection -> IO (Maybe PartnerInvite)
getInviteFromCode code conn = listToMaybe <$> query conn qs (Only code)
 where
  qs =
    "SELECT "
      <> fst inviteFields
      <> [sql| FROM tgthr.partner_invites WHERE code = ? |]

getInviteForUser :: UserID -> Connection -> IO (Maybe PartnerInvite)
getInviteForUser uid conn = listToMaybe <$> query conn qs (uid, Created)
 where
  qs =
    "SELECT "
      <> fst inviteFields
      <> [sql| FROM tgthr.partner_invites WHERE inviter_id = ? AND status = ? |]

getAllInvitesForUser :: UserID -> Connection -> IO [PartnerInvite]
getAllInvitesForUser uid conn = query conn qs (Only uid)
 where
  qs =
    "SELECT "
      <> fst inviteFields
      <> [sql| FROM tgthr.partner_invites WHERE inviter_id = ? ORDER BY created_at DESC |]

getAllPartnerInvites :: Connection -> IO [PartnerInvite]
getAllPartnerInvites conn =
  query_ conn
    $  "SELECT "
    <> fst inviteFields
    <> [sql| FROM tgthr.partner_invites ORDER BY created_at DESC |]
