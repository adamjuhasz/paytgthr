{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.Purchases.UpdateTrx where

import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           PaymentAuth.App.Purchases.CreateTransaction
                                                ( createTransaction )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                )
import           Shared.Models.Group            ( GroupModel )
import           Shared.Models.Ids              ( MessageID
                                                , TransactionId
                                                , UserID
                                                )
import           Shared.Models.Transaction      ( AptoAdjustment(..)
                                                , MerchantInfo
                                                , Transaction(..)
                                                , TransactionDetails
                                                , TransactionEvent
                                                , TransactionSource
                                                , TransactionSrcId
                                                , TransactionState(..)
                                                )

data UpdatedTrxInfo = UpdatedTrxInfo
  { utiTransaction      :: TransactionId
  , utiSource           :: TransactionSource
  , utiIdempotency      :: Maybe Text
  , utiSourceId         :: TransactionSrcId
  , utiDetails          :: TransactionDetails
  , utiMerchant         :: MerchantInfo
  , utiState            :: TransactionState
  , utiCreatedAt        :: UTCTime
  , utiTransactionEvent :: TransactionEvent
  , utiAmountLocal      :: Currency
  , utiAmountHold       :: Currency
  , utiAmountCashback   :: Currency
  , utiAmountFee        :: Currency
  , utiAmountBilling    :: Currency
  , utiDescription      :: Maybe Text
  , utiAdjustments      :: [AptoAdjustment]
  }
  deriving Show

createTrx
  :: MessageID -> UserID -> UpdatedTrxInfo -> Maybe GroupModel -> Transaction
createTrx mid uid UpdatedTrxInfo {..} group = (createTransaction
                                                mid
                                                utiTransaction
                                                TrxCreated
                                                utiTransactionEvent
                                                utiAmountLocal
                                                uid
                                                group
                                                utiSource
                                                utiSourceId
                                                (Just utiDetails)
                                                (Just utiMerchant)
                                                utiDescription
                                                utiCreatedAt
                                              )
  { trxRevision = 0
  }

updateTransaction
  :: MessageID
  -> UpdatedTrxInfo
  -> UTCTime
  -> UserID
  -> Maybe GroupModel
  -> Maybe Transaction
  -> Transaction
updateTransaction mid trxUpdates@UpdatedTrxInfo {..} now uid group prevTrx =
  let potentialCreatedTrx = createTrx mid uid trxUpdates group -- if prevTrx is empty we use this as the skeleton to update
      skeletonTrx         = fromMaybe potentialCreatedTrx prevTrx
      newState            = case (trxState <$> prevTrx, utiState) of
        (Just (TrxDeclined s), TrxDeclined _) -> TrxDeclined s -- We know more why we declined the transaction
        (_                   , _            ) -> utiState
      newAmount = case (utiState, getMonetaryValue utiAmountBilling < 0) of
        (TrxAuthorized, True) -> abs utiAmountBilling
        (TrxPending   , True) -> abs utiAmountBilling
        (TrxCompleted , _   ) -> (-1) * utiAmountBilling
        (TrxDeclined _, True) -> abs utiAmountBilling
        _                     -> trxDisplayAmount skeletonTrx
      newBilling = (utiAmountBilling, now) : trxBillingAmounts skeletonTrx
  in  skeletonTrx { trxMsgSource         = mid
                  , trxState             = newState
                  , trxRevision          = 1 + trxRevision skeletonTrx
                  , trxSourceIdempotency = utiIdempotency
                  , trxSourceEvent       = utiTransactionEvent
                  , trxDisplayAmount     = newAmount
                  , trxBillingAmounts    = newBilling
                  , trxDetails           = Just utiDetails
                  , trxMerchant          = Just utiMerchant
                  , trxAdjustments       = sortOn adjCreatedAt utiAdjustments
                  }
