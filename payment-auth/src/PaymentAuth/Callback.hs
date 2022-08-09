{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module PaymentAuth.Callback
  ( callback
  ) where

import           Control.Monad.Trans.Reader     ( runReaderT )
import qualified Data.Text                     as T
import           PaymentAuth.App.GetAccountList ( getTheAccountList )
import           PaymentAuth.App.GetBalance     ( getLiability
                                                , getSpendingLimit
                                                )
import           PaymentAuth.App.GetPlaidBalance
                                                ( getPlaidBalance )
import           PaymentAuth.App.GetRisk        ( getRiskFor )
import           PaymentAuth.App.Payments.GetPayment
                                                ( getPayment
                                                , getPaymentFromSourceId
                                                , getPaymentsCreatedAtTime
                                                )
import           PaymentAuth.App.Purchases.GetTransaction
                                                ( getRecentTransactions
                                                , getTransaction
                                                , getTransactionFromAptoId
                                                )
import           PaymentAuth.App.RiskManagement.UpdateWorkflow
                                                ( ExtraFacts(..)
                                                , riskWorkFlow
                                                )
import           PaymentAuth.AppMonad           ( AppIOM(unAppIOM)
                                                , AppSettings
                                                )
import           Shared.Amqp                    ( AMQPCallback
                                                , AMQPPublisher
                                                , CommandMessage(..)
                                                , EventMessage(..)
                                                , MessageBody(..)
                                                , ReplyMessage(..)
                                                , ReplySuccess(..)
                                                , TgthrMessage(..)
                                                , failureWithText
                                                , genSuccess
                                                )
import           Shared.Models.Currency         ( getMonetaryValue )
import           Shared.Models.Payment          ( Payment(..) )
import           Shared.Models.RiskScore        ( RiskScore(..) )
import           Shared.Models.Transaction      ( Transaction(..) )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PaymentCmd(..)
                                                , PaymentEvents(..)
                                                , PaymentReplies(..)
                                                )
import           Shared.WebAPI.General.API      ( midToTrace )

payReply :: PaymentReplies -> Maybe ReplyMessage
payReply = Just . ReplySuccessV1 . PayReplySuccess

callback :: (AMQPPublisher -> AppSettings) -> AMQPCallback
callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetSpendableBalance {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetSpendableBalance"
    trace <- midToTrace mid
    let fn = getSpendingLimit trace gbcUser
    bal <- runReaderT (unAppIOM fn) (settingsFactory pub)
    return (payReply $ GetBalanceReply gbcUser bal, [])

callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (PayCmd AddToken{}) } = do
  putStrLn "RabbitMQ CommandV1 PayCmd AddToken"
  putStrLn "Error: no longer working"
  return (Just genSuccess, [])

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetAccountList {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetAccountList"
    putStr "GetAccountList for mid: " >> print mid
    let fn = getTheAccountList gacUser
    result <- runReaderT (unAppIOM fn) (settingsFactory pub)
    putStr "GetAccountList done for mid: " >> print mid

    return $ case result of
      Left err -> (Just $ failureWithText err, [])
      Right as ->
        ( Just . ReplySuccessV1 . PayReplySuccess $ AccountListReplyV2 gacUser
                                                                       as
        , [PayEvt $ UserLinked { uleUser = gacUser }]
        )

callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (PayCmd msg@SetPrimaryBank{}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd SetPrimaryBank"
    putStr "Error: PayCmd SetPrimaryBank " >> print msg
    error "Error: PayCmd SetPrimaryBank no longer works"

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetPayment {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetPayment"
    trace <- midToTrace mid
    let fn = getPayment trace gpcPayment
    payment <- runReaderT (unAppIOM fn) (settingsFactory pub)
    case payment of
      Nothing -> do
        let s = "Error: PayCmd GetPayment gpcPayment Doesn't exists "
              <> show (mid, gpcPayment)
        return (Just $ failureWithText (T.pack s), [])
      Just p -> return
        ( Just . ReplySuccessV1 . PayReplySuccess $ GetPaymentReply gpcPayment p
        , []
        )

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetTransactionFromAptoId {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetTransactionFromAptoId"
    trace <- midToTrace mid
    let fn = getTransactionFromAptoId trace gtcAptoTransactionId
    trx <- runReaderT (unAppIOM fn) (settingsFactory pub)
    return $ case trx of
      Nothing ->
        (Just $ failureWithText "gtcAptoTransactionId Doesn't exists", [])
      Just t ->
        ( Just . ReplySuccessV1 . PayReplySuccess $ GetTransactionReply
          (trxId t)
          t
        , []
        )

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetPaymentFromSourceId {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetPaymentFromSourceId"
    trace <- midToTrace mid
    let fn = getPaymentFromSourceId trace gxcSourceId
    payment <- runReaderT (unAppIOM fn) (settingsFactory pub)
    case payment of
      Nothing -> do
        putStr "Error: gxcSourceId Doesn't exists " >> print (gxcSourceId, mid)
        let errorMsg = "gxcSourceId Doesn't exists " <> gxcSourceId
        return (Just $ failureWithText errorMsg, [])
      Just p -> return
        ( Just . ReplySuccessV1 . PayReplySuccess $ GetPaymentReply (payId p) p
        , []
        )

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetTransaction {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetTransaction"
    trace <- midToTrace mid
    let fn = getTransaction trace gtcTransaction
    trx <- runReaderT (unAppIOM fn) (settingsFactory pub)
    return $ case trx of
      Nothing -> (Just $ failureWithText "gtcTransaction Doesn't exists", [])
      Just t ->
        ( Just . ReplySuccessV1 . PayReplySuccess $ GetTransactionReply
          (trxId t)
          t
        , []
        )

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetPaymentPendingTime {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetPaymentPendingTime"
    trace <- midToTrace mid
    let fn = getPaymentsCreatedAtTime trace gycPayment
    createdAt <- runReaderT (unAppIOM fn) (settingsFactory pub)
    return $ case createdAt of
      Nothing -> (Just $ failureWithText "gycPayment Doesn't exists", [])
      Just time ->
        ( Just . ReplySuccessV1 . PayReplySuccess $ GetPaymentPendingTimeReply
          gycPayment
          time
        , []
        )

callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (PayCmd RefreshAllBalances) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd RefreshAllBalances"
    putStrLn "Error: (RefreshAllBalances) started"
    return (Just genSuccess, [])

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd RefreshBalance {..}) }
  = do
    trace <- midToTrace mid
    putStrLn "RabbitMQ CommandV1 PayCmd RefreshBalance"
    let fn = getPlaidBalance trace rbcUser
    res <- runReaderT (unAppIOM fn) (settingsFactory pub)
    return $ case res of
      Just bal -> (Just genSuccess, [PayEvt (BalanceRefreshed rbcUser bal)])
      Nothing  -> ((Just . failureWithText) "Could not refresh: ", [])

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetUsersLiability {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetUsersLiability"
    trace <- midToTrace mid
    let fn = getLiability trace glcUser
    bal <- runReaderT (unAppIOM fn) (settingsFactory pub)
    return
      ( Just . ReplySuccessV1 . PayReplySuccess $ GetUsersLiabilityReply
        glcUser
        bal
      , []
      )

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetUsersRiskScore {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetUsersRiskScore"
    trace <- midToTrace mid
    let fn = getRiskFor trace grcUser
    riskyNess <- runReaderT (unAppIOM fn) (settingsFactory pub)
    return
      ( Just . ReplySuccessV1 . PayReplySuccess $ GetUsersRiskScoreReply
        grcUser
        riskyNess
      , []
      )

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd AdjustUsersRiskScore {..}) }
  = do
    trace <- midToTrace mid
    putStrLn "RabbitMQ CommandV1 PayCmd AdjustUsersRiskScore"
    let fn = riskWorkFlow trace arcUser arcFact NoExtraInfo
    riskScoreMaybe <- runReaderT (unAppIOM fn) (settingsFactory pub)
    let riskEvent = case riskScoreMaybe of
          Nothing -> []
          Just riskScore ->
            [ PayEvt $ RiskUpdated { rueUser       = rskUser riskScore
                                   , rueTrustScore = rskTrustScore riskScore
                                   , rueChange     = rskChange riskScore
                                   , rueFact       = rskFact riskScore
                                   , rueRevision   = rskRev riskScore
                                   , rueTime       = rskCreatedAt riskScore
                                   }
            ]
    return (Just genSuccess, riskEvent)

callback settingsFactory pub mid TgthrMessage { tgthrBody = CommandV1 (PayCmd GetMostRecentTransactions {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd GetMostRecentTransactions"
    trace <- midToTrace mid
    let fn = getRecentTransactions trace gmcUser gmcCount
    trxs <- runReaderT (unAppIOM fn) (settingsFactory pub)
    let filteredTrxs = filter
          (\Transaction {..} -> getMonetaryValue trxDisplayAmount /= 0)
          trxs
    let reply = GetTransactionsReply filteredTrxs
    return (Just . ReplySuccessV1 $ PayReplySuccess reply, [])

callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (PayCmd RequestStatement {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 PayCmd RequestStatement"
    return
      ( Just genSuccess
      , [PayEvt $ StatementRequested rscUser rscRange rscLastMonths]
      )

callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (AcctCmd _) } =
  return (Nothing, [])
callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (AptoCmd _) } =
  return (Nothing, [])
callback _ _ _ TgthrMessage { tgthrBody = EventV1 _ } = return (Nothing, [])
callback _ _ _ TgthrMessage { tgthrBody = ReplyV1 _ } = return (Nothing, [])
