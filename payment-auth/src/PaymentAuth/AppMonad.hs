{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- Lazy records for easier testing
{-# LANGUAGE NoStrictData #-}


{- HLINT ignore "Reduce duplication" -}

module PaymentAuth.AppMonad where

import           Control.Concurrent             ( MVar )
import           Control.Concurrent.Async       ( async )
import           Control.Exception              ( SomeException
                                                , throw
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , asks
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(toJSON)
                                                )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Clock
import qualified Data.UUID                     as U
import           Data.UUID.V4                   ( nextRandom )
import           Network.HTTP.Client            ( Manager
                                                , Request
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking(..)
                                                , object
                                                )
import           PaymentAuth.Monad.HasEnvironment
import           PaymentAuth.Monad.HttpClient   ( HasHttpClient(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..)
                                                , IncludeVerificationPayments(..)
                                                )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           PaymentAuth.Plaid              ( Requester
                                                , getAuth
                                                )
import           PaymentAuth.Types              ( DBActions(..) )
import           Servant                        ( Handler
                                                , NoContent(..)
                                                , ServerError
                                                )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Amqp                    ( AMQPPublisher )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import qualified Shared.DB.Ledger              as LDB
import           Shared.DB.Payment              ( getPaymentsForUser )
import           Shared.Database                ( PooledDB )
import           Shared.Models.Ids              ( UserID(UserID) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry )
import           Shared.Models.Payment          ( Payment
                                                  ( Payment
                                                  , payId
                                                  , paySubType
                                                  )
                                                , PaymentSubType(NormalPayment)
                                                )
import           Shared.Models.Transaction      ( Transaction(..) )
import           Shared.Track.HasTracing        ( HasTracing(..)
                                                , trackSpan
                                                )
import           Shared.Track.Segment          as Segment
                                                ( CurrentUser(CurrentUser)
                                                , PayeeUser(PayeeUser)
                                                , trackEvent
                                                , trackOneOffTrait
                                                , trackTransactionUpdated
                                                )
import           Shared.Track.Willow            ( AttributeUpdate(..)
                                                , Routes(_UpdateAttribute)
                                                , willowClientM
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( MarkRewardUsedBody(..)
                                                , Routes(..)
                                                , accountsRoutes
                                                , getAllActiveUsersTemplate
                                                , getGroupForUserTemplate
                                                , getUserTemplate
                                                , incrementTrace
                                                , removeFundingSourceTemplate
                                                , traceToMID
                                                )
import           Shared.WebAPI.ApiDwolla.Client ( Routes(..)
                                                , dwollaRoutes
                                                )
import           Web.JWT                        ( Signer )

newtype PlaidClientID = PlaidClientID Text deriving (Eq, Show)
newtype PlaidSecret = PlaidSecret Text deriving (Eq, Show)

data AppSettings = AppSettings
  { requester      :: Requester
  , amqpPublisher  :: AMQPPublisher
  , dbAction       :: DBActions
  , accountsEnv    :: ClientEnv
  , dwollaApiEnv   :: ClientEnv
  , segmentRequest :: Request -> IO ()
  , plaidEnv       :: ClientEnv
  , plaidSecrets   :: (PlaidClientID, PlaidSecret)
  , dbPool         :: PooledDB
  , environment    :: RunningEnvironment
  , stackDriver    :: (Manager, (Text, Signer, Text))
  , isShuttingDown :: MVar Bool
  , willowEnv      :: ClientEnv
  }

newtype AppIOM a =
    AppIOM { unAppIOM :: ReaderT AppSettings IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader AppSettings
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

newtype AppWebM a =
    AppWebM { unAppWebM :: ReaderT AppSettings Handler a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader AppSettings
      , MonadError ServerError
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

instance HasTracing AppWebM where
  traceChildSpan name parentTrace fn = do
    thisTrace <- incrementTrace parentTrace
    traceSpan name parentTrace thisTrace fn
  traceSpan name parentTrace thisTrace fn = do
    tracePrint parentTrace "Tracing: " name

    startTime      <- getCurrentTime
    res            <- fn
    endTime        <- getCurrentTime

    env            <- asks environment

    (manager, jwt) <- asks stackDriver
    when
      (env == Production)
      (void . liftIO . async $ trackSpan manager
                                         jwt
                                         name
                                         parentTrace
                                         thisTrace
                                         startTime
                                         endTime
      )
    --return result
    return res

instance HasAccounts AppIOM where
  getGroupFor trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getGroupForUserTemplate trace' uid env
  getUser trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getUserTemplate trace' uid env
  removeFundingSource trace uid method failCode = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    removeFundingSourceTemplate trace' uid method failCode env
  getAllActiveUsers trace = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getAllActiveUsersTemplate trace' env
  getCategorySplits trace gid = do
    trace' <- incrementTrace trace
    env    <- asks accountsEnv
    let getCats = _GroupGetCategorySplits $ accountsRoutes env
    liftIO $ getCats trace' gid
  setBankDetails trace uid body = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let createFS = _UserCreateFS $ accountsRoutes env
    NoContent <- liftIO $ createFS trace' uid body
    return ()
  queryUsersWithBank trace details = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let query = _UsersQueryBank $ accountsRoutes env
    liftIO $ query trace' details
  verifyCurrentFS trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let verify = _UserVerifyCurrentFS $ accountsRoutes env
    NoContent <- liftIO $ verify trace' uid
    return ()
  adminlockCardsForUser trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let close = _UserLockAllCards $ accountsRoutes env
    NoContent <- liftIO $ close trace' uid
    return ()
  getRewardsForGroup trace gid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GroupGetRewards $ accountsRoutes env
    liftIO $ fn trace' gid
  getReward trace rid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _RewardsGetSpecific $ accountsRoutes env
    liftIO $ fn trace' rid
  findCard trace card = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _FindCard $ accountsRoutes env
    liftIO $ fn trace' card
  markRewardAsUsed trace (gid, rid) tid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GroupMarkRewardAsUsed $ accountsRoutes env
    NoContent <- liftIO $ fn trace' gid rid $ MarkRewardUsedBody tid
    return ()
  getReferralProgress trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GetReferralProgress $ accountsRoutes env
    liftIO $ fn trace' uid
  getReferralProgram trace rid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GetReferralProgram $ accountsRoutes env
    liftIO $ fn trace' rid
  updateReferralProgress trace uid prog = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _UpdateReferralProgress $ accountsRoutes env
    NoContent <- liftIO $ fn trace' uid prog
    return ()

instance HasHttpClient AppIOM where
  getHTTPClient = asks requester

instance HasLedgerDB AppIOM where
  getUsersWithBalances _trace = do
    pool <- asks dbPool
    let fn = withResource pool LDB.getUsersWithBalances
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      putStr "Error: LDB.getUsersWithBalances " >> print e
                      throw e
                    )
    liftIO caught
  getLedgerForIdem _trace journal idem = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getLedgerEntryForIdemKey journal idem
    let handler :: SomeException -> IO (Maybe LedgerEntry)
        handler e = do
          putStr "Error: LDB.getLedgerEntryForIdemKey " >> print (idem, e)
          throw e
    let caught = fn `catch` handler
    liftIO caught
  saveLedgerTransaction trace trx body = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.saveLedgerTransaction trx body
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      tracePrint trace "LDB.saveLedgerTransaction failed" e
                      throw e
                    )
    liftIO caught
  getLedgerJournalType _trace search = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getJournalType search
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      putStr "Error: LDB.getLedgerJournalType "
                        >> print (search, e)
                      throw e
                    )
    liftIO caught
  getLedgerJournal _trace jid = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getJournalId jid
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      putStr "Error: LDB.getLedgerJournal " >> print (jid, e)
                      throw e
                    )
    liftIO caught
  getLedgerEntry _trace leid = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getLedgerEntryId leid
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      putStr "Error: LDB.getLedgerEntry " >> print (leid, e)
                      throw e
                    )
    liftIO caught
  getEntriesForLedger _trace jid = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getEntriesForJournal jid
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      putStr "Error: LDB.getEntriesForJournal "
                        >> print (jid, e)
                      throw e
                    )
    liftIO caught

instance HasPaymentsDB AppIOM where
  getPayment _trace pid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbLoadPayment pid
  savePayment _trace payment = do
    DBActions {..} <- asks dbAction
    liftIO $ dbSavePayment payment
  getPendingPaymentsOf _trace uid includedVerifications = do
    DBActions {..} <- asks dbAction
    payments       <- liftIO $ dbGetUsersPendingPayments uid
    return $ case includedVerifications of
      IncludeVerification  -> payments
      WithoutVerifcication -> filter
        (\Payment { paySubType = subType } -> subType == NormalPayment)
        payments
  getPaymentFromSourceId _trace anId = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetPaymentFromSourceId anId
  getPaymentsCreatedAtTime _trace pid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetPendingPaymentCreatedAt pid
  getPaymentsOf _trace uid = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getPaymentsForUser uid

instance HasPlaidDB AppIOM where
  getPlaidEnv      = asks plaidEnv
  getPlaidClientId = do
    (PlaidClientID cid, _) <- asks plaidSecrets
    return cid
  getPlaidSecret = do
    (_, PlaidSecret secret) <- asks plaidSecrets
    return secret
  insertToken mid uid tuple = do
    DBActions {..} <- asks dbAction
    liftIO $ dbInsertToken mid uid tuple
  getAccessToken uid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetAccessToken uid
  getPlaidAuth tuple = do
    req <- asks requester
    getAuth req tuple
  saveBalance mid uid balance = do
    DBActions {..} <- asks dbAction
    liftIO $ dbInsertBalance mid uid balance
  getPrimaryAccount uid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetPrimaryAccount uid
  updateTokenPrimary mid uid token = do
    DBActions {..} <- asks dbAction
    liftIO $ dbUpdateTokenPrimary mid uid token
  getUsersBalanceWithin uid time = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetRecentBalanceSince uid time

instance HasRandom AppIOM where
  aRandomUUID = liftIO nextRandom

instance HasRiskScoresDB AppIOM where
  getRiskScoreOf trace uid = do
    DBActions {..} <- asks dbAction
    let mid = traceToMID trace
    liftIO $ dbGetUsersRisk mid uid
  saveRisk _trace score = do
    DBActions {..} <- asks dbAction
    liftIO $ dbSaveRiskScore score

instance HasTime AppIOM where
  getCurrentTime = liftIO Clock.getCurrentTime

instance HasTransactionsDB AppIOM where
  getTransaction _trace tid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbLoadTransaction tid
  saveTransaction _trace trx = do
    DBActions {..} <- asks dbAction
    liftIO $ dbSaveTransaction trx
  getPendingTransactionsFor _trace uid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetPendingTransactions uid
  getTransactionUsingSourceId _trace sid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbLoadTransactionFromAptoId sid
  getTransactionsFor _trace uid count = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetUsersTransactions uid count

-----------------------------------------------------------------------------
-- AppWebM
------------------------------------------------------------------------------

instance HasAccounts AppWebM where
  getGroupFor trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    traceChildSpan "PaymentAuth.Monad.HasAccounts.getGroupFor" trace
      $ getGroupForUserTemplate trace' uid env
  getUser trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    traceChildSpan "PaymentAuth.Monad.HasAccounts.getUser" trace
      $ getUserTemplate trace' uid env
  removeFundingSource trace uid method failCode = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    traceChildSpan "PaymentAuth.Monad.HasAccounts.removeFundingSource" trace
      $ removeFundingSourceTemplate trace' uid method failCode env
  getAllActiveUsers trace = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    traceChildSpan "PaymentAuth.Monad.HasAccounts.getAllActiveUsers" trace
      $ getAllActiveUsersTemplate trace' env
  getCategorySplits trace gid = do
    trace' <- incrementTrace trace
    env    <- asks accountsEnv
    let getCats = _GroupGetCategorySplits $ accountsRoutes env
    traceChildSpan "PaymentAuth.Monad.HasAccounts.getCategorySplits" trace
      $ liftIO
      $ getCats trace' gid
  setBankDetails trace uid body = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let createFS = _UserCreateFS $ accountsRoutes env
    NoContent <-
      traceChildSpan "PaymentAuth.Monad.HasAccounts.setBankDetails" trace
      $ liftIO
      $ createFS trace' uid body
    return ()
  queryUsersWithBank trace details = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let query = _UsersQueryBank $ accountsRoutes env
    traceChildSpan "PaymentAuth.Monad.HasAccounts.queryUsersWithBank" trace
      $ liftIO
      $ query trace' details
  verifyCurrentFS trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let verify = _UserVerifyCurrentFS $ accountsRoutes env
    NoContent <-
      traceChildSpan "PaymentAuth.Monad.HasAccounts.verifyCurrentFS" trace
      $ liftIO
      $ verify trace' uid
    return ()
  adminlockCardsForUser trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let close = _UserLockAllCards $ accountsRoutes env
    NoContent <-
      traceChildSpan "PaymentAuth.Monad.HasAccounts.adminlockCardsForUser" trace
      $ liftIO
      $ close trace' uid
    return ()
  getRewardsForGroup trace gid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GroupGetRewards $ accountsRoutes env
    traceChildSpan "PaymentAuth.Monad.HasAccounts.getRewardsForGroup" trace
      $ liftIO
      $ fn trace' gid
  getReward trace rid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _RewardsGetSpecific $ accountsRoutes env
    traceChildSpan "PaymentAuth.Monad.HasAccounts.getReward" trace $ liftIO $ fn
      trace'
      rid
  findCard trace card = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _FindCard $ accountsRoutes env
    traceChildSpan "PaymentAuth.Monad.HasAccounts.findCard" trace $ liftIO $ fn
      trace'
      card
  markRewardAsUsed trace (gid, rid) tid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GroupMarkRewardAsUsed $ accountsRoutes env
    NoContent <-
      traceChildSpan "PaymentAuth.Monad.HasAccounts.markRewardAsUsed" trace
      $ liftIO
      $ fn trace' gid rid
      $ MarkRewardUsedBody tid
    return ()
  getReferralProgress trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GetReferralProgress $ accountsRoutes env
    liftIO $ fn trace' uid
  getReferralProgram trace rid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _GetReferralProgram $ accountsRoutes env
    liftIO $ fn trace' rid
  updateReferralProgress trace uid prog = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _UpdateReferralProgress $ accountsRoutes env
    NoContent <- liftIO $ fn trace' uid prog
    return ()

instance HasHttpClient AppWebM where
  getHTTPClient = asks requester

instance HasLedgerDB AppWebM where
  getUsersWithBalances trace = do
    pool <- asks dbPool
    let fn = withResource pool LDB.getUsersWithBalances
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      traceError trace "Error: LDB.getUsersWithBalances " e
                      throw e
                    )
    traceChildSpan "PaymentAuth.Monad.HasLedgerDB.getUsersWithBalances" trace
      $ liftIO caught
  getLedgerForIdem trace journal idem = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getLedgerEntryForIdemKey journal idem
    let handler :: SomeException -> IO (Maybe LedgerEntry)
        handler e = do
          traceError trace "Error: LDB.getLedgerEntryForIdemKey " (idem, e)
          throw e
    let caught = fn `catch` handler
    traceChildSpan "PaymentAuth.Monad.HasLedgerDB.getLedgerForIdem" trace
      $ liftIO caught
  saveLedgerTransaction trace trx body = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.saveLedgerTransaction trx body
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      traceError trace "LDB.saveLedgerTransaction failed" e
                      throw e
                    )
    traceChildSpan "PaymentAuth.Monad.HasLedgerDB.saveLedgerTransaction" trace
      $ liftIO caught
  getLedgerJournalType trace search = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getJournalType search
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      traceError trace
                                 "Error: LDB.getLedgerJournalType "
                                 (search, e)
                      throw e
                    )
    traceChildSpan "PaymentAuth.Monad.HasLedgerDB.getLedgerJournalType" trace
      $ liftIO caught
  getLedgerJournal trace jid = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getJournalId jid
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      traceError trace "Error: LDB.getLedgerJournal " (jid, e)
                      throw e
                    )
    traceChildSpan "PaymentAuth.Monad.HasLedgerDB.getLedgerJournal" trace
      $ liftIO caught
  getLedgerEntry trace leid = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getLedgerEntryId leid
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      traceError trace "Error: LDB.getLedgerEntry " (leid, e)
                      throw e
                    )
    traceChildSpan "PaymentAuth.Monad.HasLedgerDB.getLedgerEntry" trace
      $ liftIO caught
  getEntriesForLedger trace jid = do
    pool <- asks dbPool
    let fn = withResource pool $ LDB.getEntriesForJournal jid
    let caught =
          fn
            `catch` (\(e :: SomeException) -> do
                      traceError trace
                                 "Error: LDB.getEntriesForJournal "
                                 (jid, e)
                      throw e
                    )
    traceChildSpan "PaymentAuth.Monad.HasLedgerDB.getEntriesForLedger" trace
      $ liftIO caught


instance HasPaymentsDB AppWebM where
  getPayment trace pid = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasPaymentsDB.getPayment" trace
      $ liftIO
      $ dbLoadPayment pid
  savePayment trace payment = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasPaymentsDB.savePayment" trace
      $ liftIO
      $ dbSavePayment payment
  getPendingPaymentsOf trace uid includedVerifications = do
    DBActions {..} <- asks dbAction
    payments       <-
      traceChildSpan "PaymentAuth.Monad.HasPaymentsDB.getPendingPaymentsOf"
                     trace
      $ liftIO
      $ dbGetUsersPendingPayments uid
    return $ case includedVerifications of
      IncludeVerification  -> payments
      WithoutVerifcication -> filter
        (\Payment { paySubType = subType } -> subType == NormalPayment)
        payments
  getPaymentFromSourceId trace anId = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasPaymentsDB.getPaymentFromSourceId"
                   trace
      $ liftIO
      $ dbGetPaymentFromSourceId anId
  getPaymentsCreatedAtTime trace pid = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasPaymentsDB.getPaymentsCreatedAtTime"
                   trace
      $ liftIO
      $ dbGetPendingPaymentCreatedAt pid
  getPaymentsOf trace uid = do
    pool <- asks dbPool
    traceChildSpan "PaymentAuth.Monad.HasPaymentsDB.getPaymentsOf" trace
      $ liftIO
      $ withResource pool
      $ getPaymentsForUser uid

instance HasPlaidDB AppWebM where
  getPlaidEnv      = asks plaidEnv
  getPlaidClientId = do
    (PlaidClientID cid, _) <- asks plaidSecrets
    return cid
  getPlaidSecret = do
    (_, PlaidSecret secret) <- asks plaidSecrets
    return secret
  insertToken mid uid tuple = do
    DBActions {..} <- asks dbAction
    liftIO $ dbInsertToken mid uid tuple
  getAccessToken uid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetAccessToken uid
  getPlaidAuth tuple = do
    req <- asks requester
    getAuth req tuple
  saveBalance mid uid balance = do
    DBActions {..} <- asks dbAction
    liftIO $ dbInsertBalance mid uid balance
  getPrimaryAccount uid = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetPrimaryAccount uid
  updateTokenPrimary mid uid token = do
    DBActions {..} <- asks dbAction
    liftIO $ dbUpdateTokenPrimary mid uid token
  getUsersBalanceWithin uid time = do
    DBActions {..} <- asks dbAction
    liftIO $ dbGetRecentBalanceSince uid time

instance HasRandom AppWebM where
  aRandomUUID = liftIO nextRandom

instance HasRiskScoresDB AppWebM where
  getRiskScoreOf trace uid = do
    DBActions {..} <- asks dbAction
    let mid = traceToMID trace
    traceChildSpan "PaymentAuth.Monad.HasRiskScoresDB.getRiskScoreOf" trace
      $ liftIO
      $ dbGetUsersRisk mid uid
  saveRisk trace score = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasRiskScoresDB.saveRisk" trace
      $ liftIO
      $ dbSaveRiskScore score

instance HasTime AppWebM where
  getCurrentTime = liftIO Clock.getCurrentTime

instance HasTransactionsDB AppWebM where
  getTransaction trace tid = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasTransactionsDB.getTransaction" trace
      $ liftIO
      $ dbLoadTransaction tid
  saveTransaction trace trx = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasTransactionsDB.saveTransaction" trace
      $ liftIO
      $ dbSaveTransaction trx
  getPendingTransactionsFor trace uid = do
    DBActions {..} <- asks dbAction
    traceChildSpan
        "PaymentAuth.Monad.HasTransactionsDB.getPendingTransactionsFor"
        trace
      $ liftIO
      $ dbGetPendingTransactions uid
  getTransactionUsingSourceId trace sid = do
    DBActions {..} <- asks dbAction
    traceChildSpan
        "PaymentAuth.Monad.HasTransactionsDB.getTransactionUsingSourceId"
        trace
      $ liftIO
      $ dbLoadTransactionFromAptoId sid
  getTransactionsFor trace uid count = do
    DBActions {..} <- asks dbAction
    traceChildSpan "PaymentAuth.Monad.HasTransactionsDB.getTransactionsFor"
                   trace
      $ liftIO
      $ dbGetUsersTransactions uid count

instance HasEventTracking AppWebM where
  trackEventWithProps mid userId eventName props
    | userId == UserID U.nil = return ()
    | otherwise = do
      segmentRequester <- asks segmentRequest
      userModelMaybe   <- getUser mid userId
      userModel        <- case userModelMaybe of
        Nothing ->
          error
            $  "Error: Could not get user inside trackEventWithProps "
            <> show (userId, eventName)
        Just u -> return u
      liftIO . void . async $ Segment.trackEvent segmentRequester userModel eventName props
  trackEvent mid userId eventName
    | userId == UserID U.nil = return ()
    | otherwise = trackEventWithProps mid userId eventName $ object []
  trackOneOffTrait userId traits
    | userId == UserID U.nil = return ()
    | otherwise = do
      segmentRequester <- asks segmentRequest
      liftIO . void . async $ Segment.trackOneOffTrait segmentRequester userId traits
      willowE <- asks willowEnv
      liftIO . void . async $ do
        let willowUpdate = _UpdateAttribute
              willowClientM
              "Bearer aaaaaaabbbbbbbcd"
              (AttributeUpdate
                "user"
                (T.pack . show $ userId)
                (object $ foldr (\(t, v) accum -> accum <> [t .= v]) [] traits)
                []
              )
        _ <- runClientM willowUpdate willowE
        return ()

  trackTransactionChanged trace prevState trx = do
    trace'           <- incrementTrace trace
    segmentRequester <- asks segmentRequest
    willowE          <- asks willowEnv

    let payeeId       = trxUserId trx
    let affectedUsers = fst <$> trxSplitAmounts trx

    payeeM <- getUser trace' payeeId
    case payeeM of
      Nothing    -> return ()
      Just payee -> forM_ affectedUsers $ \currentUser -> do
        thisUserM <- getUser trace' currentUser
        case thisUserM of
          Nothing       -> return ()
          Just thisUser ->  liftIO . void . async $ do
            Segment.trackTransactionUpdated segmentRequester
                                            (CurrentUser thisUser)
                                            (PayeeUser payee)
                                            prevState
                                            trx
            let
              willowUpdate = _UpdateAttribute
                willowClientM
                "Bearer aaaaaaabbbbbbbcd"
                (AttributeUpdate
                  "purchase"
                  (T.pack . show $ trxId trx)
                  (toJSON trx)
                  ( map (\(u, _) -> ("user", T.pack . show $ u))
                  $ trxSplitAmounts trx
                  )
                )
            _ <- runClientM willowUpdate willowE
            return ()

instance HasEventTracking AppIOM where
  trackEventWithProps trace userId eventName props = do
    trace'           <- incrementTrace trace
    segmentRequester <- asks segmentRequest
    userModelMaybe   <- getUser trace' userId
    userModel        <- case userModelMaybe of
      Nothing ->
        error $ "Error: Could not get user inside trackEventWithProps " <> show
          (userId, eventName)
      Just u -> return u
    liftIO . void . async $ Segment.trackEvent segmentRequester userModel eventName props
  trackEvent mid userId eventName =
    trackEventWithProps mid userId eventName $ object []
  trackOneOffTrait userId traits = do
    segmentRequester <- asks segmentRequest
    liftIO . void . async $ Segment.trackOneOffTrait segmentRequester userId traits
    willowE <- asks willowEnv
    liftIO . void . async $ do
      let willowUpdate = _UpdateAttribute
            willowClientM
            "Bearer aaaaaaabbbbbbbcd"
            (AttributeUpdate
              "user"
              (T.pack . show $ userId)
              (object $ foldr (\(t, v) accum -> accum <> [t .= v]) [] traits)
              []
            )
      _ <- runClientM willowUpdate willowE
      return ()
  trackTransactionChanged trace prevState trx = do
    trace'           <- incrementTrace trace
    segmentRequester <- asks segmentRequest
    willowE          <- asks willowEnv

    let payeeId       = trxUserId trx
    let affectedUsers = fst <$> trxSplitAmounts trx

    payeeM <- getUser trace' payeeId
    case payeeM of
      Nothing    -> return ()
      Just payee -> forM_ affectedUsers $ \currentUser -> do
        thisUserM <- getUser trace' currentUser
        case thisUserM of
          Nothing       -> return ()
          Just thisUser -> liftIO . void . async $ do
            Segment.trackTransactionUpdated segmentRequester
                                            (CurrentUser thisUser)
                                            (PayeeUser payee)
                                            prevState
                                            trx
            let
              willowUpdate = _UpdateAttribute
                willowClientM
                "Bearer aaaaaaabbbbbbbcd"
                (AttributeUpdate
                  "purchase"
                  (T.pack . show $ trxId trx)
                  (toJSON trx)
                  ( map (\(u, _) -> ("user", T.pack . show $ u))
                  $ trxSplitAmounts trx
                  )
                )
            _ <- runClientM willowUpdate willowE
            return ()

instance HasDwollaClient AppWebM where
  initiatePayment trace Payment {..} = do
    env       <- asks dwollaApiEnv
    trace'    <- incrementTrace trace
    NoContent <- liftIO $ _PaymentInitiate (dwollaRoutes env) trace' payId
    return ()
  cancelPayment trace pid = do
    env       <- asks dwollaApiEnv
    trace'    <- incrementTrace trace
    NoContent <- liftIO $ _PaymentCancel (dwollaRoutes env) trace' pid
    return ()
