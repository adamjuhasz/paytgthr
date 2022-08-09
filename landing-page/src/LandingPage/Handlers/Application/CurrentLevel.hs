module LandingPage.Handlers.Application.CurrentLevel where

import           Data.Aeson                     ( KeyValue((.=))
                                                , Value
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( nil )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Handlers.Transactions.Utils
                                                ( calculateMaximim
                                                , calculateMinimum
                                                )
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Currency         ( Currency
                                                , getMonetaryValue
                                                , roundDownUSD
                                                )
import           Shared.Models.Group            ( GroupMember(..)
                                                , GroupModel(..)
                                                , GroupSplit(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.Ids              ( GroupId(..)
                                                , MessageID(..)
                                                )
import           Shared.Models.RiskScore        ( RiskFact(..)
                                                , RiskScore(..)
                                                , UserLevel(..)
                                                , dollarToNextLevel
                                                , expectedSpendPerLevel
                                                , getUserLevel
                                                , limitByLevel
                                                , progressToNextLevel
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , accountsClientM
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( Routes(..)
                                                , payAuthClientM
                                                , paymentauthRoutes
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                , setHeader
                                                )

currencyToDbl :: Currency -> Double
currencyToDbl = fromRational . getMonetaryValue

roundedDbl :: Currency -> Double
roundedDbl = fromRational . getMonetaryValue . roundDownUSD

levelToNum :: UserLevel -> Int
levelToNum lev = case lev of
  Level0 -> 0
  Level1 -> 1
  Level2 -> 2
  Level3 -> 3
  Level4 -> 4
  Level5 -> 5

limitBuilder :: UserLevel -> Value
limitBuilder lev = object
  [ "number" .= levelToNum lev
  , "limitUSD" .= currencyToDbl (limitByLevel lev)
  , "purchasesTillNext"
    .= (roundToInt . currencyToDbl . expectedSpendPerLevel $ lev)
  , "name" .= show (levelToNum lev)
  ]
 where
  roundToInt :: Double -> Int
  roundToInt = ceiling

-- >>> import Shared.Models.RiskScore
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 0 }
-- 1
--
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 10 }
-- 75
--
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 24.1 }
-- 5
--
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 25 }
-- 150
--
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 50 }
-- 500
--
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 75 }
-- 1000
--
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 90 }
-- 401
--
-- >>> dollarsToNextLevel $ exampleRiskScore {rskTrustScore = 99.99 }
-- 0
--
dollarsToNextLevel :: RiskScore -> Int
dollarsToNextLevel riskScore =
  ceiling . getMonetaryValue $ dollarToNextLevel riskScore

getCurrentLevel :: V.Key SessionData -> ClientEnv -> ClientEnv -> ActionM ()
getCurrentLevel sKey payAuthEnv accountsEnv = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace
  now   <- liftAndCatchIO getCurrentTime

  let defaultRiskScore u = RiskScore { rskUser       = u
                                     , rskRev        = 1
                                     , rskTrustScore = 10
                                     , rskChange     = 0
                                     , rskFact       = InitialRisk
                                     , rskMsgSource  = MessageID nil
                                     , rskCreatedAt  = now
                                     }

  let defaultGroup = GroupModel
        { grpId        = GroupId nil
        , grpStatus    = GroupCreated
        , grpStart     = Nothing
        , grpEnd       = Nothing
        , grpSplit     = [GroupSplit user 50 True, GroupSplit user 50 True]
        , grpMembers   = [GroupMember user True, GroupMember user True]
        , grpRevision  = 1
        , grpVersion   = "1.0"
        , grpMsgSource = MessageID nil
        , grpCreatedAt = now
        }

  let getRiskScore = _GetTrustScore payAuthClientM trace
  let getActiveGroupsClient =
        _GroupsForUser accountsClientM trace user [GroupActive]
  let getSpendingLimitClient = _GetSpendableBalance payAuthClientM trace
  let getLedgerValue = _GetLiability (paymentauthRoutes payAuthEnv) trace

  let getGroupForUser = do
        res <- runClientM getActiveGroupsClient accountsEnv
        case res of
          Left e -> do
            putStr "Error: _GroupsForUser " >> print (user, e)
            return $ Left $ show e
          Right []      -> return $ Left "No group found"
          Right (g : _) -> return $ Right g

  let getSpendingLimit aperson = do
        res <- runClientM (getSpendingLimitClient aperson) payAuthEnv
        case res of
          Left e -> do
            putStr "Error: _GetSpendableBalance " >> print (aperson, e)
            return $ Left $ show e
          Right c -> return $ Right c

  let getRisk u = do
        riskScoreE <- runClientM (getRiskScore u) payAuthEnv
        return $ case riskScoreE of
          Left _ -> defaultRiskScore u
          Right RiskScore { rskFact = InitialRisk, rskTrustScore = 0 } ->
            defaultRiskScore u-- show a better demo for new users
          Right r -> r

  -- actual running code

  riskScore     <- liftAndCatchIO $ getRisk user

  myGroupEither <- liftAndCatchIO getGroupForUser
  let myGroup = case myGroupEither of
        Left  _  -> defaultGroup
        Right gm -> gm

  safeToSpend <- liftAndCatchIO $ calculateMinimum myGroup getSpendingLimit

  maxCanSpend <- liftAndCatchIO $ calculateMaximim myGroup getRisk

  let groupMembers = mbrUser <$> grpMembers myGroup
  ledgerBalances <- liftAndCatchIO $ mapM getLedgerValue groupMembers

  setHeader "cache-control" "private, max-age=1"
  Scotty.json $ object
    [ "currentLevel" .= levelToNum (getUserLevel riskScore)
    , "maxSpend" .= roundedDbl maxCanSpend
    , "canSpend" .= roundedDbl safeToSpend
    , "hasSpent" .= roundedDbl (sum ledgerBalances)
    , "currentProgress" .= progressToNextLevel riskScore
    , "dollarsToNextLevel" .= dollarsToNextLevel riskScore
    , "levels"
      .= [ limitBuilder Level1
         , limitBuilder Level2
         , limitBuilder Level3
         , limitBuilder Level4
         , limitBuilder Level5
         ]
    , "version" .= (1 :: Int)
    ]
