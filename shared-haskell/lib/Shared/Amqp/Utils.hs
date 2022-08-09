{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

{-|
Module      : AMQP Utils
Description : Random functions that help
Maintainer  : adam@example.com
Stability   : experimental
-}
module Shared.Amqp.Utils where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( getCurrentTime )
import qualified Data.UUID                     as U
import           Shared.Amqp                    ( AMQPPublisher
                                                , CommandMessage
                                                  ( AcctCmd
                                                  , PayCmd
                                                  )
                                                , MessageBody(ReplyV1)
                                                , ReplyMessage(ReplySuccessV1)
                                                , ReplySuccess
                                                  ( AcctReplySuccess
                                                  , PayReplySuccess
                                                  )
                                                )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Group            ( GroupModel )
import           Shared.Models.Payment          ( Payment
                                                , PaymentId
                                                )
import           Shared.Models.RiskScore        ( RiskScore )
import           Shared.Models.Transaction      ( Transaction
                                                , TransactionId
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , UserID(..)
                                                , UserModel
                                                , defaultUser
                                                )
import           Shared.TgthrMessages.Accounts  ( AccountReplies(..)
                                                , AccountsCmd(..)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PaymentCmd(..)
                                                , PaymentReplies(..)
                                                )

replyToEither :: MessageBody -> Either () ()
replyToEither x = case x of
  ReplyV1 (ReplySuccessV1 _) -> Right ()
  _                          -> Left ()

getUser :: AMQPPublisher -> UserID -> IO (Either Text UserModel)
getUser pub userid
  | userid == UserID U.nil = do
    now <- getCurrentTime
    return . Right $ defaultUser now (EmailAddress "")
  | otherwise = do
    res <- pub (AcctCmd $ GetUser userid)
    return $ case res of
      (ReplyV1 (ReplySuccessV1 (AcctReplySuccess GetUserReply { gurUserModel = model })))
        -> Right model
      x -> Left . T.pack $ "Error: " <> show x

getPayment :: AMQPPublisher -> PaymentId -> IO (Either Text Payment)
getPayment pub paymentId = do
  res <- pub (PayCmd $ GetPayment paymentId)
  return $ case res of
    (ReplyV1 (ReplySuccessV1 (PayReplySuccess GetPaymentReply { gprPaymentModel = model })))
      -> Right model
    x -> Left . T.pack $ "Error: " <> show x

getTransaction
  :: AMQPPublisher -> TransactionId -> IO (Either Text Transaction)
getTransaction pub transactionId = do
  res <- pub (PayCmd $ GetTransaction transactionId)
  return $ case res of
    (ReplyV1 (ReplySuccessV1 (PayReplySuccess GetTransactionReply { gtrTransactionModel = model })))
      -> Right model
    x -> Left . T.pack $ "Error: " <> show x

data GroupErrorResult
  = UnknownGError Text
  | GroupMissing
  deriving(Eq, Show)

getGroupForUser
  :: AMQPPublisher -> UserID -> IO (Either GroupErrorResult GroupModel)
getGroupForUser pub uid = extractGroup
  <$> pub (AcctCmd $ GetUsersActivePendingGroups uid)
 where
  extractGroup :: MessageBody -> Either GroupErrorResult GroupModel
  extractGroup (ReplyV1 (ReplySuccessV1 (AcctReplySuccess GetUserGroupReply { gxrGroups = [] })))
    = Left GroupMissing
  extractGroup (ReplyV1 (ReplySuccessV1 (AcctReplySuccess GetUserGroupReply { gxrGroups = grp : _ })))
    = Right grp
  extractGroup _ = Left $ UnknownGError "User Missing"

getGroupsForUser
  :: AMQPPublisher -> UserID -> IO (Either GroupErrorResult [GroupModel])
getGroupsForUser pub uid = extractGroups
  <$> pub (AcctCmd $ GetUsersActivePendingGroups uid)
 where
  extractGroups :: MessageBody -> Either GroupErrorResult [GroupModel]
  extractGroups (ReplyV1 (ReplySuccessV1 (AcctReplySuccess GetUserGroupReply { gxrGroups })))
    = Right gxrGroups
  extractGroups _ = Left $ UnknownGError "User Missing"

getSpendingLimit :: AMQPPublisher -> UserID -> IO (Either Text Currency)
getSpendingLimit pub user = do
  res <- pub (PayCmd $ GetSpendableBalance user)
  return $ case res of
    (ReplyV1 (ReplySuccessV1 (PayReplySuccess GetBalanceReply {..}))) ->
      Right gbrBalance
    x -> Left . T.pack $ "Error: " <> show x

getRiskScore :: AMQPPublisher -> UserID -> IO (Either Text RiskScore)
getRiskScore pub user = do
  res <- pub (PayCmd $ GetUsersRiskScore user)
  return $ case res of
    (ReplyV1 (ReplySuccessV1 (PayReplySuccess GetUsersRiskScoreReply {..}))) ->
      Right grrRiskScore
    x -> Left . T.pack $ "Error: " <> show x

getLastTransactions
  :: AMQPPublisher -> UserID -> IO (Either Text [Transaction])
getLastTransactions pub uid = do
  res <- pub (PayCmd $ GetMostRecentTransactions uid 1000)
  return $ case res of
    (ReplyV1 (ReplySuccessV1 (PayReplySuccess GetTransactionsReply { grrTransactions = models })))
      -> Right models
    x -> Left . T.pack $ "Error: " <> show x
