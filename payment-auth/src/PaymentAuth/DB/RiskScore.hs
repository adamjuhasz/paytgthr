{-# LANGUAGE QuasiQuotes #-}

module PaymentAuth.DB.RiskScore where

import           Data.Maybe                     ( listToMaybe )
import           Data.Time.Clock                ( getCurrentTime )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(Only)
                                                , execute
                                                , query
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.RiskScore        ( RiskFact(InitialRisk)
                                                , RiskScore(..)
                                                , riskScoreFields
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )

getUsersRisk :: MessageID -> UserID -> Connection -> IO RiskScore
getUsersRisk mid uid conn = do
  existingScore <- listToMaybe <$> query conn qs selector
  case existingScore of
    Just score -> return score
    Nothing    -> do
      now <- getCurrentTime
      let newScore = defaultRisk now
      saveRiskScore newScore conn
      return newScore
 where
  qs =
    "SELECT "
      <> fst riskScoreFields
      <> [sql| FROM tgthr.riskscores_current_rev WHERE user_id = ? |]
  selector = Only uid
  defaultRisk now = RiskScore { rskUser       = uid
                              , rskRev        = 1
                              , rskTrustScore = 0
                              , rskChange     = 0
                              , rskFact       = InitialRisk
                              , rskMsgSource  = mid
                              , rskCreatedAt  = now
                              }

saveRiskScore :: RiskScore -> Connection -> IO ()
saveRiskScore risk conn = withTransaction conn $ do
  _ <- execute conn insert risk
  _ <- execute conn upsert risk
  return ()
 where
  insert =
    "INSERT INTO tgthr.riskscores ("
      <> fst riskScoreFields
      <> ") VALUES ("
      <> snd riskScoreFields
      <> ")"

  upsert =
    "UPSERT INTO tgthr.riskscores_current_rev ("
      <> fst riskScoreFields
      <> ") VALUES ("
      <> snd riskScoreFields
      <> ")"

