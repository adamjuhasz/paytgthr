{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module PaymentAuth.App.Purchases.UpdateTransaction
  ( updateTransaction
  ) where

import           Control.Monad                  ( forM )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadMask
                                                , SomeException
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Purchases.AuthorizeTransaction
                                                ( IsGroupOk(GroupNotOk, GroupOK)
                                                , isGroupOk
                                                )
import qualified PaymentAuth.App.Purchases.UpdateTrx
                                               as UpdateTrx
import           PaymentAuth.App.Purchases.Workflow.Update.UpdateLedger
                                                ( updateLedgerWorkflow )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Shared.Console                 ( traceError )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupSplit(..)
                                                )
import           Shared.Models.Ids              ( UserID(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

-- |The 'isUserQualified' checks if a user is active and has a verified funding source
isUserQualified
  :: Maybe UserModel -- ^ results from GetGroupForUser
  -> Bool
isUserQualified Nothing = False
isUserQualified (Just UserModel { usrUserState = UserActive, usrBankVerified = Just True })
  = True
isUserQualified (Just _) = False

type PreviousTransactionState = TransactionState
updateTransaction
  :: ( HasAccounts m
     , HasLedgerDB m
     , HasTime m
     , HasTransactionsDB m
     , HasRandom m
     , MonadIO m
     , MonadMask m
     )
  => UserID
  -> TraceContext
  -> UpdateTrx.UpdatedTrxInfo
  -> m (PreviousTransactionState, Transaction, [LedgerEntry])
updateTransaction uid trace updates = do
  let mid = traceToMID trace
  let tid = UpdateTrx.utiTransaction updates

  -- find group
  groupE   <- getGroupFor trace uid
  trxMaybe <- getTransaction trace tid
  now      <- getCurrentTime

  group    <- case isGroupOk now groupE of
    -- verify group, if needed make solo trx 
    GroupNotOk    -> return Nothing
    -- verify partner, if needed make solo trx
    GroupOK model -> do
      let users = splUser <$> grpSplit model
      usersE <- forM users (getUser trace)
      let allUsersQualed = and $ fmap isUserQualified usersE
      if allUsersQualed then return $ Just model else return Nothing

  let prevTrxState = maybe TrxCreated trxState trxMaybe
  let updatedTrx =
        UpdateTrx.updateTransaction mid updates now uid group trxMaybe

  saveTransaction trace updatedTrx

  let catcher :: (MonadIO m) => a -> SomeException -> m a
      catcher def e = do
        traceError trace "Error: could not run update trx workflow " (show e)
        return def

  ledgerEntrys <- updateLedgerWorkflow trace updatedTrx `catch` catcher []

  return (prevTrxState, updatedTrx, fst <$> ledgerEntrys)
