{-# LANGUAGE RecordWildCards, StrictData, DeriveAnyClass, DeriveGeneric, NamedFieldPuns #-}

module Shared.Models.RiskScore where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID
                                                , nil
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           GHC.Stack                      ( HasCallStack )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                , roundUp
                                                )
import           Shared.Models.Ids              ( MessageID(..)
                                                , PaymentId
                                                , UserID(..)
                                                )
import           Shared.Models.Transaction      ( TransactionId(..) )
import           Shared.Utils                   ( customAesonOptions
                                                , stringToTime
                                                )

newtype RiskId = RiskId UUID deriving (Eq, Show, Generic, FromJSON, ToJSON)

data RiskFact
  = InitialRisk
  | ManualRiskAdj Double
  | FailedPayment PaymentId
  | SuccessfulPayment PaymentId
  | ChangedLinkedAcct
  | RiskyAcctBalance Currency
  | RiskyTransaction TransactionId
  deriving(Eq, Show, Generic)

data UserLevel
  = Level0
  | Level1
  | Level2
  | Level3
  | Level4
  | Level5
  deriving (Eq, Ord, Show, Generic)

data RiskScore = RiskScore
  { rskUser       :: UserID
  , rskRev        :: Int
  , rskTrustScore :: Double -- 1 is no trust (Scammer) -> 100 is full trust
  , rskChange     :: Double
  , rskFact       :: RiskFact
  , rskMsgSource  :: MessageID
  , rskCreatedAt  :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToField RiskId where
  toField (RiskId anID) = toField anID
instance FromField RiskId where
  fromField a dat = RiskId <$> fromField a dat

instance FromJSON RiskFact where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON RiskFact where
  toJSON = genericToJSON customAesonOptions
instance ToField RiskFact where
  toField = toJSONField
instance FromField RiskFact where
  fromField = fromJSONField

instance FromJSON RiskScore where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON RiskScore where
  toJSON = genericToJSON customAesonOptions
instance ToRow RiskScore where
  toRow RiskScore {..} =
    [ toField rskUser
    , toField rskRev
    , toField rskTrustScore
    , toField rskChange
    , toField rskFact
    , toField rskMsgSource
    , toField rskCreatedAt
    ]

instance FromRow RiskScore where
  fromRow = do
    rskUser       <- field
    rskRev        <- field
    rskTrustScore <- field
    rskChange     <- field
    rskFact       <- field
    rskMsgSource  <- field
    rskCreatedAt  <- field
    return RiskScore { .. }

riskScoreFields :: (Query, Query)
riskScoreFields =
  ( " user_id, revision, score, change, fact, msg_source, created_at "
  , " ?, ?, ?, ?, ?, ?, ? "
  )

instance FromJSON UserLevel where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON UserLevel where
  toJSON = genericToJSON customAesonOptions

getUserLevel :: RiskScore -> UserLevel
getUserLevel RiskScore { rskTrustScore } = getUserLevelFromScore rskTrustScore

getUserLevelFromScore :: HasCallStack => Double -> UserLevel
getUserLevelFromScore rskTrustScore
  | rskTrustScore < trustBaseForLevel Level1 = Level0
  | rskTrustScore < trustBaseForLevel Level2 = Level1
  | rskTrustScore < trustBaseForLevel Level3 = Level2
  | rskTrustScore < trustBaseForLevel Level4 = Level3
  | rskTrustScore < trustBaseForLevel Level5 = Level4
  | rskTrustScore >= trustBaseForLevel Level5 = Level5
  | otherwise = error $ "Unknown level for " <> show rskTrustScore

trustBaseForLevel :: UserLevel -> Double
trustBaseForLevel Level0 = 0
trustBaseForLevel Level1 = 10
trustBaseForLevel Level2 = 25
trustBaseForLevel Level3 = 50
trustBaseForLevel Level4 = 75
trustBaseForLevel Level5 = 100

nextLevel :: UserLevel -> UserLevel
nextLevel Level0 = Level1
nextLevel Level1 = Level2
nextLevel Level2 = Level3
nextLevel Level3 = Level4
nextLevel Level4 = Level5
nextLevel Level5 = Level5

riskAdjustedLimit :: RiskScore -> Currency
riskAdjustedLimit = limitByLevel . getUserLevel

limitByLevel :: UserLevel -> Currency
limitByLevel Level0 = Currency "USD" 0
limitByLevel Level1 = Currency "USD" 75   -- risk very-high
limitByLevel Level2 = Currency "USD" 150  -- risk medium
limitByLevel Level3 = Currency "USD" 500  -- risk medium
limitByLevel Level4 = Currency "USD" 1000 -- risk low
limitByLevel Level5 = Currency "USD" 2500 -- risk very low

expectedSpendPerLevel :: UserLevel -> Currency
expectedSpendPerLevel Level0 = Currency "USD" 1    -- N/A can't divide by zero
expectedSpendPerLevel Level1 = Currency "USD" 75   -- spend $150 as a couple to move up to level 2
expectedSpendPerLevel Level2 = Currency "USD" 150  -- spend $450 as a couple to move up to level 3
expectedSpendPerLevel Level3 = Currency "USD" 500  -- spend $1450 as a couple to move up to level 4
expectedSpendPerLevel Level4 = Currency "USD" 1000 -- spend $2450 as a couple to move up to Level 5
expectedSpendPerLevel Level5 = Currency "USD" 1    -- N/A max level

-- >>> riskAdvancer (Currency "USD" 75) (getUserLevelFromScore 10)
-- 15.0
-- 
-- >>> levScore = trustBaseForLevel Level1
-- >>> getUserLevelFromScore $ levScore + riskAdvancer (Currency "USD" 75) (getUserLevelFromScore levScore)
-- Level2
--
-- >>> levScore = trustBaseForLevel Level2
-- >>> getUserLevelFromScore $ levScore + riskAdvancer (Currency "USD" 75) (getUserLevelFromScore levScore)
-- Level2
--
-- >>> levScore = trustBaseForLevel Level2
-- >>> riskAdvancer (Currency "USD" 75) (getUserLevelFromScore levScore)
-- 12.5
--
-- >>> levScore = trustBaseForLevel Level2
-- >>> getUserLevelFromScore $ levScore + riskAdvancer (expectedSpendPerLevel Level2) (getUserLevelFromScore levScore)
-- Level3
--
-- >>> levScore = trustBaseForLevel Level3
-- >>> getUserLevelFromScore $ levScore + riskAdvancer (expectedSpendPerLevel Level3) (getUserLevelFromScore levScore)
-- Level4
--
-- >>> levScore = trustBaseForLevel Level4
-- >>> getUserLevelFromScore $ levScore + riskAdvancer (expectedSpendPerLevel Level4) (getUserLevelFromScore levScore)
-- Level5
--
-- >>> levScore = trustBaseForLevel Level5
-- >>> getUserLevelFromScore $ levScore + riskAdvancer (Currency "USD" 100) (getUserLevelFromScore levScore)
-- Level5
riskAdvancer :: Currency -> UserLevel -> Double
riskAdvancer _ Level0 = 0 -- let them get spending again
riskAdvancer _ Level5 = 0 -- max
riskAdvancer amount level =
  let trustToNextLevel =
        trustBaseForLevel (nextLevel level) - trustBaseForLevel level
  in  fromRational
        $ truncate' 1
        $ (*) (toRational trustToNextLevel)
        $ getMonetaryValue
        $ amount
        / expectedSpendPerLevel level

-- | Truncate to `n` decimal places
truncate' :: Int -> Rational -> Rational
truncate' n x =
  let t :: Rational  = 10 ^ n
      floored :: Int = floor (x * t)
      normalized     = toRational floored / t
  in  normalized

round' :: Integer -> Double -> Double
round' sg num = ((fromIntegral :: Integer -> Double) . round $ num * f) / f
  where f = 10 ^ sg

-- >>> progressToNextLevel exampleRiskScore { rskTrustScore = 11 } 
-- 7.0e-2
--
-- >>> progressToNextLevel exampleRiskScore { rskTrustScore = 24.8 }
-- 0.99
--
-- >>> progressToNextLevel exampleRiskScore { rskTrustScore = 25 }
-- 0.0
--
-- >>> progressToNextLevel exampleRiskScore { rskTrustScore = 64.12 }
-- 0.56
--
-- >>> progressToNextLevel exampleRiskScore { rskTrustScore = 79.3 }
-- 0.17
--
-- >>> progressToNextLevel exampleRiskScore { rskTrustScore = 95 }
-- 0.8
--
-- >>> progressToNextLevel exampleRiskScore { rskTrustScore = 1 }
-- 0.1
--
progressToNextLevel :: RiskScore -> Double
progressToNextLevel RiskScore { rskTrustScore = 100 } = 1.0
progressToNextLevel RiskScore { rskTrustScore = 0 }   = 0.0
progressToNextLevel RiskScore { rskTrustScore } =
  let currentLevel    = getUserLevelFromScore rskTrustScore
      levelAbove      = nextLevel currentLevel
      thisLevelsFloor = trustBaseForLevel currentLevel
      nextLevelFloor  = trustBaseForLevel levelAbove
      levelSpacing    = nextLevelFloor - thisLevelsFloor
  in  if levelSpacing <= 0
        then 0
        else round' 2 $ (rskTrustScore - thisLevelsFloor) / levelSpacing

exampleRiskScore :: RiskScore
exampleRiskScore = RiskScore
  { rskUser       = UserID nil
  , rskRev        = 1
  , rskTrustScore = 10
  , rskChange     = 10
  , rskFact       = InitialRisk
  , rskMsgSource  = MessageID nil
  , rskCreatedAt  = stringToTime "2019-09-02T20:15:32+00:00"
  }

-- | Money needed to next level
--
-- >>> dollarToNextLevel exampleRiskScore { rskTrustScore = 10}
-- Currency "USD" (75 % 1)
--
-- >>> dollarToNextLevel exampleRiskScore { rskTrustScore = 25}
-- Currency "USD" (150 % 1)
--
-- >>> dollarToNextLevel exampleRiskScore { rskTrustScore = 29}
-- Currency "USD" (126 % 1)
--
-- >>> dollarToNextLevel exampleRiskScore { rskTrustScore = 10}
-- Currency "USD" (75 % 1)
--
-- >>> dollarToNextLevel exampleRiskScore { rskTrustScore = 95}
-- Currency "USD" (200 % 1)
--
-- >>> dollarToNextLevel exampleRiskScore { rskTrustScore = 100}
-- Currency "USD" (0 % 1)
--
-- >>> dollarToNextLevel exampleRiskScore { rskTrustScore = 0}
-- Currency "USD" (1 % 1)
--
dollarToNextLevel :: RiskScore -> Currency
dollarToNextLevel risk =
  let progressStillToGo = 1 - progressToNextLevel risk
  in  roundUp 2
        $ Currency "USD" (toRational progressStillToGo)
        * expectedSpendPerLevel (getUserLevel risk)
