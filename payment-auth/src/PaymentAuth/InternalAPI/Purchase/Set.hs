{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Reduce duplication" -}

module PaymentAuth.InternalAPI.Purchase.Set where

import           PaymentAuth.App.Purchases.UpdateState
                                                ( setTransactionState )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Transaction      ( TransactionId(..)
                                                , TransactionState(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

setPurcahseState
  :: (HasAccounts m, HasTransactionsDB m)
  => TraceContext
  -> TransactionId
  -> TransactionState
  -> m NoContent
setPurcahseState trace trxId newState = do
  setTransactionState trace trxId newState
  return NoContent

