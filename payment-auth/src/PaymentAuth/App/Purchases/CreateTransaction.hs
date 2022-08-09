{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.Purchases.CreateTransaction where

import           Control.Monad                  ( msum )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupSplit(..)
                                                )
import           Shared.Models.Ids              ( MessageID
                                                , TransactionId
                                                , UserID
                                                )
import           Shared.Models.Transaction      ( MerchantInfo
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionEvent
                                                , TransactionSource
                                                , TransactionSrcId
                                                , TransactionState
                                                )

createTransaction
  :: MessageID
  -> TransactionId
  -> TransactionState
  -> TransactionEvent
  -> Currency
  -> UserID
  -> Maybe GroupModel
  -> TransactionSource
  -> TransactionSrcId
  -> Maybe TransactionDetails
  -> Maybe MerchantInfo
  -> Maybe Text
  -> UTCTime
  -> Transaction
createTransaction mid trxId state event amount uid group src srcId details merchant description purchasedAt
  = let currentSplits = case group of
          Nothing -> [(uid, 100)]
          -- @todo replace with cat splits
          Just g  -> grpSplit g <&> (\GroupSplit {..} -> (splUser, splRatio))
        normalizedDesc =
          msum
            [ description
            , details >>= pcpDescription
            , Just "Pending Transaction"
            ]
        trxRevision          = 1
        trxVersion           = "1.0"
        trxMsgSource         = mid
        trxState             = state
        trxSource            = src
        trxSourceId          = srcId
        trxSourceEvent       = event
        trxUserId            = uid
        trxDisplayAmount     = abs amount
        trxBillingAmounts    = []
        trxDetails           = details
        trxGroupId           = group <&> (\g -> (grpId g, grpRevision g))
        trxSourceIdempotency = Nothing
        trxSplitAmounts      = currentSplits
        trxMerchant          = merchant
        trxDescription       = normalizedDesc
        trxPurchasedAt       = purchasedAt
        trxAdjustments       = []
        trxRewardId          = Nothing
    in  Transaction { .. }
