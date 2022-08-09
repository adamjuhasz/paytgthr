{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module PaymentAuth where

import           Control.Concurrent             ( forkIO
                                                , newEmptyMVar
                                                , putMVar
                                                , readMVar
                                                , threadDelay
                                                )
import           Control.Concurrent.Async       ( async
                                                , cancel
                                                , link
                                                )
import           Control.Exception              ( evaluate )
import           Control.Monad                  ( void )
import           Control.Once                   ( once )
import           Control.Retry                  ( RetryStatus(..)
                                                , constantDelay
                                                , limitRetries
                                                , recoverAll
                                                )
import qualified Data.ByteString.Char8         as BC
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( unpack )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.AMQP                   ( ExchangeOpts(..)
                                                , QueueOpts(..)
                                                , bindQueue
                                                , declareExchange
                                                , declareQueue
                                                , newExchange
                                                , newQueue
                                                )
import           Network.Connection             ( TLSSettings(..) )
import           Network.HTTP.Client            ( applyBasicAuth
                                                , httpLbs
                                                , newManager
                                                )
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , getPort
                                                , runSettings
                                                , setPort
                                                )
import           Network.Wai.Handler.WarpTLS    ( runTLS
                                                , tlsSettingsMemory
                                                )
import           Network.Wai.Middleware.Gzip    ( GzipFiles(..)
                                                , GzipSettings(..)
                                                , def
                                                , gzip
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( OutputFormat(..)
                                                , RequestLoggerSettings(..)
                                                , mkRequestLogger
                                                )
import           PaymentAuth.AppMonad           ( AppSettings(..)
                                                , PlaidClientID(..)
                                                , PlaidSecret(..)
                                                )
import           PaymentAuth.Callback           ( callback )
import           PaymentAuth.DB                 ( getAccessToken
                                                , getPlaidTokenRow
                                                , getPrimaryAccount
                                                , getRecentBalanceSince
                                                , getTokens
                                                , getUserFromItem
                                                , insertBalance
                                                , insertPlaidTokenRow
                                                , insertToken
                                                , updateTokenPrimary
                                                )
import qualified PaymentAuth.DB.Migrations     as Migrations
import           PaymentAuth.DB.RiskScore       ( getUsersRisk
                                                , saveRiskScore
                                                )
import           PaymentAuth.DB.Transaction     ( getAllPendingTransactions
                                                , getPendingTransactions
                                                , getUsersTransactions
                                                , loadTransaction
                                                , loadTransactionFromAptoId
                                                , saveTransaction
                                                )
import           PaymentAuth.ExternalAPI        ( ApplicationConfig(..)
                                                , Environment(..)
                                                , app
                                                )
import           PaymentAuth.InternalAPI        ( internalApiApp )
import qualified PaymentAuth.Monad.HasEnvironment
                                               as HA
import           PaymentAuth.Plaid              ( PlaidSecrets(..)
                                                , createMananger
                                                , generateRequest
                                                )
import           PaymentAuth.Plaid.API          ( generateClientEnv )
import           PaymentAuth.Types              ( DBActions(..) )
import           Servant.Client                 ( mkClientEnv
                                                , parseBaseUrl
                                                )
import           Shared.Amqp                    ( AMQPCallback
                                                , AMQPPublisher
                                                , addConsumer
                                                , getAMQPChan
                                                , publish
                                                , startChannel
                                                )
import           Shared.DB.Ledger               ( getUsersWithBalances
                                                , saveLedgerEntry
                                                )
import           Shared.DB.Payment              ( getPaymentFromSourceId
                                                , getPendingPaymentCreatedAt
                                                , getUsersPendingPayments
                                                , loadPayment
                                                , savePayment
                                                )
import           Shared.Database                ( createDBPool )
import           Shared.Database.Utils          ( ekgRunner
                                                , timedSampler
                                                )
import           Shared.Track.Stackdriver       ( getStackdriverSecrets
                                                , stackDriverMiddleware
                                                )
import           Shared.Vault                   ( VaultCertResponse(..)
                                                , VaultCredResponse(..)
                                                , extractSecret
                                                , getPKICert
                                                , getRMQCreds
                                                , getSecretOrThrow
                                                , initManager
                                                , readTokenFile
                                                )
import           Shared.Web.RequestLogger       ( formatAsJSONWithHeaders )
import           Shared.Web.Utils               ( warpDefaultSettings )
import           System.Metrics                 ( createDistribution
                                                , newStore
                                                )
import           System.Posix.Signals           ( Handler(Catch)
                                                , installHandler
                                                , sigTERM
                                                )

data PaymentAuthSettings = PaymentAuthSettings
  { desiredPort :: String
  , rmqRole     :: String
  , rmqAddr     :: String
  , vBase       :: String
  , tokenFile   :: String
  , ekgPort     :: String
  }

retryFunc :: IO b -> RetryStatus -> IO b
retryFunc f RetryStatus { rsIterNumber = 0 } = f
retryFunc f r = do
  putStrLn ("PaymentAuth Error Retry: " <> show r)
  f

retryCallback :: (AMQPPublisher -> AppSettings) -> AMQPCallback
retryCallback a b c d = recoverAll (constantDelay 50000 <> limitRetries 2)
  $ retryFunc (callback a b c d)

mainPaymentAuth :: PaymentAuthSettings -> IO ()
mainPaymentAuth PaymentAuthSettings {..} = do
  -- EKG
  store     <- newStore
  _         <- forkIO $ timedSampler store
  storeOnce <- once $ \p n ->
    print (p <> n <> "_ms") >> createDistribution (p <> n <> "_ms") store

  vToken       <- readTokenFile tokenFile
  tlsManager   <- initManager
  (rmqU, rmpP) <- getRMQCreds tlsManager vBase vToken rmqRole <&> \c ->
    case c of
      Left  e                      -> error e
      Right VaultCredResponse {..} -> (unpack vcUsername, unpack vcPassword)
  let secretGetter = getSecretOrThrow tlsManager vBase vToken
  plaidClientSecrets <- secretGetter "payment-auth/plaid"
  cdbSecrets         <- secretGetter "payment-auth/cockroach"
  let plaidClientID = extractSecret plaidClientSecrets "client_id"
  let plaidSecret   = extractSecret plaidClientSecrets "secret"
  let plaidURL      = extractSecret plaidClientSecrets "url"
  let psqlString    = extractSecret cdbSecrets "psql_string"

  sdInfo <- getStackdriverSecrets secretGetter

  putStr "RMQ Username: " >> print rmqU
  let amqpString = "amqp://" <> rmqU <> ":" <> rmpP <> "@" <> rmqAddr

  let plaidSecrets = PlaidSecrets { clientId = T.pack plaidClientID
                                  , secret   = T.pack plaidSecret
                                  , url      = plaidURL
                                  }

  (plaidManager, _) <- createMananger plaidSecrets
  let request = generateRequest plaidSecrets plaidManager

  -- DB Setup
  psqlPool <- createDBPool psqlString
  let runner = ekgRunner storeOnce psqlPool "payment.db."
  let runUpdTokPrim a b c =
        runner "updateTokenPrimary" (updateTokenPrimary a b c)
  let runGetBalSince a b =
        runner "getRecentBalanceSince" (getRecentBalanceSince a b)
  let runGetUsrTrx a b =
        runner "getUsersTransactions" (getUsersTransactions a b)
  let
    dbActions = DBActions
      { dbInsertBalance = \a b c -> runner "insertBalance" (insertBalance a b c)
      , dbGetTokens = runner "getTokens" getTokens
      , dbGetPrimaryAccount = runner "getPrimaryAccount" . getPrimaryAccount
      , dbGetAccessToken = runner "getAccessToken" . getAccessToken
      , dbGetPlaidTokenRow = runner "getPlaidTokenRow" . getPlaidTokenRow
      , dbInsertPlaidTokenRow = runner "insertPlaidTokenRow"
                                  . insertPlaidTokenRow
      , dbUpdateTokenPrimary = runUpdTokPrim
      , dbInsertToken = \a b c -> runner "insertToken" (insertToken a b c)
      , dbGetRecentBalanceSince = runGetBalSince
      , dbGetUserFromItem = runner "getUserFromItem" . getUserFromItem
      , dbSaveTransaction = runner "saveTransaction" . saveTransaction
      , dbLoadTransaction = runner "loadTransaction" . loadTransaction
      , dbLoadTransactionFromAptoId = runner "loadTransactionFromAptoId"
                                        . loadTransactionFromAptoId
      , dbGetPendingTransactions = runner "getPendingTransactions"
                                     . getPendingTransactions
      , dbSavePayment = runner "savePayment" . savePayment
      , dbLoadPayment = runner "loadPayment" . loadPayment
      , dbGetPaymentFromSourceId = runner "getPaymentFromSourceId"
                                     . getPaymentFromSourceId
      , dbGetUsersPendingPayments = runner "getUsersPendingPayments"
                                      . getUsersPendingPayments
      , dbGetPendingPaymentCreatedAt = runner "getPendingPaymentCreatedAt"
                                         . getPendingPaymentCreatedAt
      , dbSaveLedgerEntry = runner "saveLedgerEntry" . saveLedgerEntry
      , dbGetUsersWithBalances = runner "getUsersWithBalances"
                                        getUsersWithBalances
      , dbGetAllPendingTransactions = runner "getAllPendingTransactions"
                                             getAllPendingTransactions
      , dbGetUsersRisk = \a b -> runner "getUsersRisk" (getUsersRisk a b)
      , dbSaveRiskScore = runner "getUsersRisk" . saveRiskScore
      , dbGetUsersTransactions = runGetUsrTrx
      }

  -- Databse Migrations
  withResource psqlPool Migrations.addPaymentACHInfo
  withResource psqlPool Migrations.createNewCurrentRevTable

  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = True
        , settingDisableSession               = False
        , settingUseServerName                = False
        }
  manager     <- newManager $ mkManagerSettings tlsSettings Nothing
  accountsEnv <- mkClientEnv manager
    <$> parseBaseUrl "https://accounts-fsm-web.default.svc.cluster.local:443"
  dwollaApiEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://apidwolla-web-internal.default.svc.cluster.local:443"

  segmentSecrets <- secretGetter "tracking"
  let trackingEnv  = extractSecret segmentSecrets "environment"
  let segmentToken = extractSecret segmentSecrets "segment_token"
  -- force evaluation b/c AppConfig is lazy
  segmentRequester <- evaluate $ case trackingEnv of
    "safari" ->
      void . flip httpLbs tlsManager . applyBasicAuth (BC.pack segmentToken) ""
    _ -> const $ do
      putStrLn "Not sending to segment..."
      return ()

  plaidEnv  <- generateClientEnv plaidURL
  willowEnv <- mkClientEnv manager
    <$> parseBaseUrl "https://willow-support.fly.dev"

  -- pushing data into `toDie` will cause the race to finish
  -- https://stackoverflow.com/a/45846292/369198
  isShuttingDown <- newEmptyMVar
  shutdownNow    <- newEmptyMVar
  let sigtermHandler = Catch $ do
        putStrLn "SIGTERM received..."
        putMVar isShuttingDown True
        threadDelay 70000000 -- 70 seconds
        putStrLn "SIGTERM received... Shutting down now"
        putMVar shutdownNow True

  _ <- installHandler sigTERM sigtermHandler Nothing

  let appSettings pub = AppSettings
        { requester      = request
        , amqpPublisher  = pub
        , dbAction       = dbActions
        , accountsEnv    = accountsEnv
        , dwollaApiEnv   = dwollaApiEnv
        , segmentRequest = segmentRequester
        , plaidEnv       = plaidEnv
        , plaidSecrets   = ( PlaidClientID $ T.pack plaidClientID
                           , PlaidSecret $ T.pack plaidSecret
                           )
        , dbPool         = psqlPool
        , environment    = case trackingEnv of
                             "safari" -> HA.Production
                             _        -> HA.Staging
        , stackDriver    = sdInfo
        , isShuttingDown = isShuttingDown
        , willowEnv      = willowEnv
        }

  let myQueue = "paymentauth"
  chan <- startChannel (Just "payment-auth") amqpString
  declareExchange
    (getAMQPChan chan)
    newExchange { exchangeName    = "tgthr"
                , exchangeType    = "topic"
                , exchangeDurable = True
                }
  _ <- declareQueue (getAMQPChan chan) newQueue { queueName = myQueue }
  bindQueue (getAMQPChan chan) myQueue "tgthr" "paymentauth.cmd.#"
  bindQueue (getAMQPChan chan) myQueue "tgthr" "dwolla.event.#"
  _ <- addConsumer myQueue (retryCallback appSettings) chan

  let webSettings = ApplicationConfig { environment    = Development
                                      , sendMessage    = publish chan
                                      , db             = dbActions
                                      , isShuttingDown = isShuttingDown
                                      }

  let warpSettings = warpDefaultSettings & setPort (Prelude.read desiredPort)
  putStr "Starting External API on port " >> print (getPort warpSettings)
  externalAPI <- async (app webSettings >>= runSettings warpSettings)

  certEither  <- getPKICert
    tlsManager
    vBase
    vToken
    "payment-auth-web-internal.default.svc.cluster.local"
  VaultCertResponse {..} <- case certEither of
    Left e -> do
      putStr "Error getting certs: " >> print e
      error "Can't get cert"
    Right c -> return c
  putStr "Cert: " >> print serial_number
  let tlsOpts =
        tlsSettingsMemory (encodeUtf8 certificate) (encodeUtf8 private_key)
  let warpOpts = setPort 8443 defaultSettings
  logger <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                       formatAsJSONWithHeaders
    }

  let noAMQP _ = error "No AMQP allowed"
  internalAPI <-
    async
    . runTLS tlsOpts warpOpts
    . stackDriverMiddleware sdInfo
    . logger
    . gzip (def { gzipFiles = GzipCompress })
    . internalApiApp
    $ appSettings noAMQP

  link externalAPI
  link internalAPI

  -- wait for kill signal
  _ <- readMVar shutdownNow

  -- kill
  putStrLn "SIGTERM received... Killing Internal, External APIs"

  cancel internalAPI
  cancel externalAPI

  putStrLn "SIGTERM received... All Done"
