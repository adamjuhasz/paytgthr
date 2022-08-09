module Scaffolding.Transactions where

import           Data.Ratio                     ( (%) )
import           Data.Time.Clock                ( getCurrentTime )
import qualified Data.UUID                     as U
import           Data.UUID.V4                   ( nextRandom )
import           Scaffolding.Groups             ( basicGroup )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupModel(grpId)
                                                )
import           Shared.Models.Transaction      ( CardNetwork(..)
                                                , MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionEvent(..)
                                                , TransactionId(..)
                                                , TransactionSource(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
import           Shared.Utils                   ( stringToTime )
import           System.IO.Unsafe               ( unsafePerformIO )

basicTrx :: Transaction
basicTrx = Transaction
  { trxId                = unsafePerformIO (TransactionId <$> nextRandom)
  , trxRevision          = 1
  , trxVersion           = "1.0"
  , trxMsgSource         = MessageID U.nil
  , trxState             = TrxCreated
  , trxSource            = PayWithPrivacy
  , trxSourceId          = "trx_1"
  , trxSourceEvent       = AuthRequest
  , trxUserId            = usrUserID userJohn
  , trxDisplayAmount     = Currency "USD" 100
  , trxBillingAmounts    = []
  , trxPurchasedAt       = unsafePerformIO getCurrentTime
  , trxDetails           = Nothing
  , trxGroupId           = Just (grpId basicGroup, 1)
  , trxSourceIdempotency = Nothing
  , trxSplitAmounts      = [(usrUserID userJohn, 50), (usrUserID userJane, 50)]
  , trxMerchant          = Nothing
  , trxDescription       = Just "ABC INC"
  , trxAdjustments       = []
  , trxRewardId          = Nothing
  }

asosAmount :: Currency
asosAmount = Currency "USD" 20.97

asos :: Transaction
asos = Transaction
  { trxId                = unsafePerformIO (TransactionId <$> nextRandom)
  , trxRevision          = 1
  , trxVersion           = "1.0"
  , trxMsgSource         = unsafePerformIO (MessageID <$> nextRandom)
  , trxState             = TrxPending
  , trxSource            = Apto
  , trxSourceId          = "txn_a"
  , trxSourceEvent       = StateTransition
  , trxUserId            = usrUserID userJohn
  , trxDisplayAmount     = asosAmount
  , trxBillingAmounts    = [ ( Currency "USD" (0 % 1)
                             , stringToTime "2019-11-04T21:27:22.205856632Z"
                             )
                           ]
  , trxPurchasedAt       = stringToTime "2019-11-04T21:27:20Z"
  , trxDetails           = Just
                             (CardTransaction
                               { pcpContext         =
                                 "Decline. International decline. (MASTERCARD)"
                               , pcpIsCardPresent   = False
                               , pcpIsOnline        = True
                               , pcpIsInternational = True
                               , pcpNetwork         = Mastercard
                               , pcpIsEMV           = False
                               , pcpType            = Nothing
                               , pcpDescription = Just "WWW.ASOS.COM LILLE FRA"
                               }
                             )
  , trxGroupId           = Just (unsafePerformIO (GroupId <$> nextRandom), 9)
  , trxSourceIdempotency = Nothing
  , trxSplitAmounts      = [ (usrUserID userJohn, 60 % 1)
                           , (usrUserID userJane, 40 % 1)
                           ]
  , trxMerchant          = Just
                             (CardMerchant { cmiMcc      = MastercardMCC "5651"
                                           , cmiMccDesc = "Family Clothing Stores"
                                           , cmiName     = "WWW.ASOS.COM"
                                           , cmiLocality = Just "LILLE"
                                           , cmiRegion   = Nothing
                                           , cmiCountry  = "USA"
                                           }
                             )
  , trxDescription       = Just "WWW.ASOS.COM LILLE FRA"
  , trxAdjustments       = []
  , trxRewardId          = Nothing
  }
