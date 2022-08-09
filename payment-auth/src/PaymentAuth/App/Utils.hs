{-# LANGUAGE RecordWildCards, StrictData #-}
module PaymentAuth.App.Utils where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           PaymentAuth.App.Split          ( calculateSplit )
import           Shared.Models.Currency         ( Currency(..)
                                                , getIsoCode
                                                , getMonetaryValue
                                                )
import           Shared.Models.Transaction      ( AptoAdjustment(..)
                                                , AptoAdjustmentType(..)
                                                , DeclineReason
                                                  ( BalanceCheckFail
                                                  )
                                                , Transaction(..)
                                                , TransactionState(TrxDeclined)
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult(..) )

foldAdjustments :: AptoAdjustment -> Currency -> Currency
foldAdjustments AptoAdjustment { adjType = AuthorizationAdjustment } accum =
  accum
foldAdjustments AptoAdjustment {..} accum = adjAmountBilling + accum

unionAuthResult :: [Either AuthResult a] -> Either AuthResult [a]
unionAuthResult []  = error "non empty"
unionAuthResult arr = foldr combineAuthResult (Right []) arr

combineAuthResult
  :: Either AuthResult a -> Either AuthResult [a] -> Either AuthResult [a]
combineAuthResult (Left (BalanceRequestFailed trx x)) (Left (BalanceRequestFailed _ y))
  = Left . BalanceRequestFailed balanceFailTrx $ x <> y -- assume trx are the same
 where
  balanceFailTrx = trx { trxState = TrxDeclined (BalanceCheckFail $ x <> y) }
combineAuthResult (Left (BalanceRequestFailed trx x)) (Right _) = Left
  (BalanceRequestFailed balanceFailTrx x)
  where balanceFailTrx = trx { trxState = TrxDeclined (BalanceCheckFail x) }
combineAuthResult (Right _) (Left (BalanceRequestFailed trx x)) = Left
  (BalanceRequestFailed balanceFailTrx x)
  where balanceFailTrx = trx { trxState = TrxDeclined (BalanceCheckFail x) }
combineAuthResult (Right x) (Right ys) = Right (x : ys)
combineAuthResult _         _          = error "unknown inputs"

calcFairShares :: Transaction -> [(UserID, Currency)]
calcFairShares Transaction {..} =
  let currISO   = getIsoCode trxDisplayAmount
      currValue = getMonetaryValue trxDisplayAmount * (-1) -- display is always positive
      curryConv (a, b) = (a, Currency currISO b)
  in  curryConv <$> calculateSplit currValue trxSplitAmounts

sumBalances :: (UserID, Currency) -> Map UserID Currency -> Map UserID Currency
sumBalances (uid, bal) = Map.insertWith (+) uid bal

addBalancesWithPendingTrx
  :: [Transaction] -> [(UserID, Currency)] -> [(UserID, Currency)]
addBalancesWithPendingTrx trxs balances =
  let splits = concatMap calcFairShares trxs
  in  Map.toList $ foldr sumBalances Map.empty (splits <> balances)
