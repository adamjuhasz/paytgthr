{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module LandingPage.Handlers.Transactions.Utils where

import qualified Data.Either                   as E
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupSplit(splRatio, splUser)
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.RiskScore        ( RiskScore
                                                , getUserLevel
                                                , limitByLevel
                                                )

calculateMinimum
  :: GroupModel -> (UserID -> IO (Either b Currency)) -> IO Currency
calculateMinimum GroupModel {..} getUsersLimit = do
  let defaultLimit = Currency "USD" 0
  let limiter      = E.fromRight defaultLimit

  let members      = splUser <$> grpSplit
  let splits       = splRatio <$> grpSplit
  limitReses <- mapM getUsersLimit members

  let zipped = zip splits $ fmap limiter limitReses
  let normalized =
        fmap (\(split, lim) -> lim / (fromRational split / 100)) zipped

  return . max 0 $ minimum normalized

calculateMaximim :: GroupModel -> (UserID -> IO RiskScore) -> IO Currency
calculateMaximim GroupModel {..} getUserRisk = do
  let members = splUser <$> grpSplit
  let splits  = splRatio <$> grpSplit
  limitReses <- mapM getUserRisk members

  let zipped = zip splits $ fmap (limitByLevel . getUserLevel) limitReses
  let normalized =
        fmap (\(split, lim) -> lim / (fromRational split / 100)) zipped

  return . max 0 $ minimum normalized
