{-# LANGUAGE QuasiQuotes #-}

module Chewpaca.DB.RiskScores where

import           Data.Maybe                     ( listToMaybe )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids
import           Shared.Models.RiskScore

getCurrentRiskForUser :: UserID -> Connection -> IO (Maybe RiskScore)
getCurrentRiskForUser uid conn = listToMaybe <$> query conn qs (Only uid)
 where
  qs = "SELECT " <> fst riskScoreFields <> [sql| 
        FROM tgthr.riskscores_current_rev 
        WHERE user_id = ? 
      |]

getRisksForUser :: UUID -> Connection -> IO [RiskScore]
getRisksForUser uid conn = query conn qs selector
 where
  qs =
    "SELECT "
      <> fst riskScoreFields
      <> " FROM tgthr.riskscores WHERE user_id = ? ORDER BY revision DESC "
  selector = Only uid
