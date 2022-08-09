{-# LANGUAGE QuasiQuotes #-}

module Shared.DB.Referral where

import           Data.Maybe
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(..)
                                                , execute
                                                , query
                                                , query_
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids
import           Shared.Models.Referral.ReferralCode
import           Shared.Models.Referral.ReferralProgram
import           Shared.Models.Referral.ReferralProgress

setReferralProgram :: ReferralProgram -> Connection -> IO ()
setReferralProgram program conn = withTransaction conn $ do
  _ <- execute conn insert program
  _ <- execute conn upsert program
  return ()
 where
  upsert =
    [sql| UPSERT INTO tgthr.referral_programs ( |]
      <> fst referralProgramFields
      <> [sql| ) VALUES ( |]
      <> snd referralProgramFields
      <> ")"
  insert =
    [sql| INSERT INTO tgthr.referral_programs_historical ( |]
      <> fst referralProgramFields
      <> [sql| ) VALUES ( |]
      <> snd referralProgramFields
      <> ")"

getReferralProgram
  :: ReferralProgramID -> Connection -> IO (Maybe ReferralProgram)
getReferralProgram rid conn = listToMaybe <$> query conn qs (Only rid)
 where
  qs =
    [sql| SELECT |]
      <> fst referralProgramFields
      <> [sql| FROM tgthr.referral_programs WHERE id = ? |]

getAllReferralPrograms :: Connection -> IO [ReferralProgram]
getAllReferralPrograms conn = query_ conn qs
 where
  qs =
    [sql| SELECT |]
      <> fst referralProgramFields
      <> [sql| FROM tgthr.referral_programs ORDER BY updated_at DESC|]

getReferralCode :: ReferralCodeDisplay -> Connection -> IO (Maybe ReferralCode)
getReferralCode code conn = listToMaybe <$> query conn qs (Only code)
 where
  qs =
    [sql| SELECT |]
      <> fst referralCodeFields
      <> [sql| FROM tgthr.referral_codes WHERE code = ? |]

getReferralCodeForUser :: UserID -> Connection -> IO (Maybe ReferralCode)
getReferralCodeForUser uid conn = listToMaybe <$> query conn qs (Only uid)
 where
  qs =
    [sql| SELECT |]
      <> fst referralCodeFields
      <> [sql| FROM tgthr.referral_codes WHERE referrer_id = ? ORDER by created_at DESC |]

setReferralCode :: ReferralCode -> Connection -> IO ()
setReferralCode code conn = do
  _ <- execute conn qs code
  return ()
 where
  qs =
    [sql| UPSERT INTO tgthr.referral_codes ( |]
      <> fst referralCodeFields
      <> [sql| ) VALUES ( |]
      <> snd referralCodeFields
      <> [sql| ) |]

getReferralProgressOf :: UserID -> Connection -> IO (Maybe ReferralProgress)
getReferralProgressOf uid conn = listToMaybe <$> query conn qs (Only uid)
 where
  qs =
    [sql| SELECT |]
      <> fst referralProgressFields
      <> [sql| FROM tgthr.referral_progress WHERE referee_id = ? |]

getReferralProgressRevisions
  :: ReferralProgressID -> Connection -> IO [ReferralProgress]
getReferralProgressRevisions pid conn = query conn qs (Only pid)
 where
  qs =
    [sql| SELECT |]
      <> fst referralProgressFields
      <> [sql| FROM tgthr.referral_progress_historical WHERE id = ? ORDER BY updated_at DESC |]

getReferreeProgressFor :: UserID -> Connection -> IO [ReferralProgress]
getReferreeProgressFor uid conn = query conn qs (Only uid)
 where
  qs =
    [sql| SELECT |]
      <> fst referralProgressFields
      <> [sql| FROM tgthr.referral_progress WHERE referrer_id = ? |]

setReferralProgress :: ReferralProgress -> Connection -> IO ()
setReferralProgress prog conn = withTransaction conn $ do
  _ <- execute conn insert prog
  _ <- execute conn upsert prog
  return ()
 where
  upsert =
    [sql| UPSERT INTO tgthr.referral_progress ( |]
      <> fst referralProgressFields
      <> [sql| ) VALUES ( |]
      <> snd referralProgressFields
      <> [sql| ) |]
  insert =
    [sql| INSERT INTO tgthr.referral_progress_historical ( |]
      <> fst referralProgressFields
      <> [sql| ) VALUES ( |]
      <> snd referralProgressFields
      <> [sql| ) |]

getAllReferralProgress :: Connection -> IO [ReferralProgress]
getAllReferralProgress conn = query_ conn qs
 where
  qs =
    [sql| SELECT |]
      <> fst referralProgressFields
      <> [sql| FROM tgthr.referral_progress ORDER BY created_at DESC |]

getAllReferralCodes :: Connection -> IO [ ReferralCode]
getAllReferralCodes conn = query_ conn qs
 where
  qs =
    [sql| SELECT |]
      <> fst referralCodeFields
      <> [sql| FROM tgthr.referral_codes ORDER BY created_at DESC |]