{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AccountsFSM where

import           AFSM.AppMonad                  ( AppConfig(..) )
import           AFSM.Callback                  ( callback )
import           AFSM.Cognito.Client            ( newCognitoManager )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.DB.GroupGet               ( getGroupById
                                                , getGroupsByUserId
                                                , getGroupsByUserIdFiltered
                                                )
import           AFSM.DB.GroupSave              ( saveGroup )
import qualified AFSM.DB.Migrations            as Migrations
import           AFSM.DB.Tokens                 ( findToken
                                                , saveToken
                                                )
import           AFSM.DB.UserGet                ( findUsersWithSSN
                                                , getAccountByCardholder
                                                , getAccountByEmail
                                                , getAccountById
                                                , getAccountByPhone
                                                , getAccountsWithBankInfo
                                                , getAllActiveAccounts
                                                )
import           AFSM.DB.UserSave               ( saveUser )
import           AFSM.Monad.HasEnvironment      ( RunningEnvironment(..) )
import           AFSM.WebServer                 ( webserver )
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
import           Control.Once                   ( once )
import           Control.Retry                  ( exponentialBackoff
                                                , limitRetries
                                                , recoverAll
                                                )
import qualified Data.ByteString.Char8         as BC
import           Data.Functor                   ( (<&>) )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( unpack )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.AMQP                   ( ExchangeOpts(..)
                                                , QueueOpts(queueName)
                                                , bindQueue
                                                , declareExchange
                                                , declareQueue
                                                , newExchange
                                                , newQueue
                                                )
import           Network.Connection             ( TLSSettings(..) )
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Network.Wai.Handler.Warp       ( defaultSettings
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
import           Servant.Client                 ( mkClientEnv
                                                , parseBaseUrl
                                                )
import           Shared.Amqp                    ( AMQPCallback
                                                , addConsumer
                                                , getAMQPChan
                                                , retryAMQPCallback
                                                , startChannel
                                                )
import           Shared.Database                ( createDBPool )
import           Shared.Database.Utils          ( ekgRunner
                                                , timedSampler
                                                )
import           Shared.Track.Segment           ( createSegmentRequestor )
import           Shared.Track.Stackdriver       ( getStackdriverSecrets
                                                , stackDriverMiddleware
                                                )
import           Shared.Vault                   ( KeyName(KeyName)
                                                , VaultCertResponse(..)
                                                , VaultCredResponse(..)
                                                , decrypt
                                                , encrypt
                                                , extractSecret
                                                , getPKICert
                                                , getRMQCreds
                                                , getSecretOrThrow
                                                , initManager
                                                , postEncryption
                                                , readTokenFile
                                                )
import           Shared.Vault.SSN               ( getSSNSecrets )
import           Shared.Web.RequestLogger       ( formatAsJSONWithHeaders )
import           System.Metrics                 ( createDistribution
                                                , newStore
                                                )
import           System.Posix.Signals           ( Handler(Catch)
                                                , installHandler
                                                , sigTERM
                                                )

data AccountsFSMSettings = AccountsFSMSettings
  { rmqRole   :: String
  , rmqAddr   :: String
  , vBase     :: String
  , tokenFile :: String
  }

retryCallback :: AppConfig -> AMQPCallback
retryCallback a b c d =
  recoverAll (exponentialBackoff 5000 <> limitRetries 4)
    $ retryAMQPCallback "AccountsFSM" (callback a) b c d

mainAccountsFSM :: AccountsFSMSettings -> IO ()
mainAccountsFSM AccountsFSMSettings {..} = do
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
  cdbSecrets <- secretGetter "accounts-fsm/cockroach"
  let psqlString = extractSecret cdbSecrets "psql_string"

  putStr "RMQ Username: " >> print rmqU
  let amqpString = "amqp://" <> rmqU <> ":" <> rmpP <> "@" <> rmqAddr

  psqlPool <- createDBPool psqlString

  let runner = ekgRunner storeOnce psqlPool "afsm.db."
  let runGetGrp x y =
        runner "getGroupsByUserIdFiltered" $ getGroupsByUserIdFiltered x y
  let runUsersWActNum x y =
        runner "getUsersWithAcountNumber" $ getAccountsWithBankInfo x y
  let runFindTok x y = runner "findToken" $ findToken x y
  let
    dbActions = DBActions
      { cSaveUser                  = runner "saveUser" . saveUser
      , cGetAccountByEmail = runner "getAccountByEmail" . getAccountByEmail
      , cGetAccountByPhone = runner "getAccountByPhone" . getAccountByPhone
      , cGetAccountById            = runner "getAccountById" . getAccountById
      , cGetAccounByCardholder     = runner "getUserByCardholder"
                                       . getAccountByCardholder
      , cSaveGroup                 = runner "saveGroup" . saveGroup
      , cGetGroupById              = runner "getGroupById" . getGroupById
      , cGetGroupsByUserIdFiltered = runGetGrp
      , cGetGroupsByUserId         = runner "getGroupsByUserIdFiltered"
                                       . getGroupsByUserId
      , cGetAllActiveUsers = runner "getAllActiveUsers" getAllActiveAccounts
      , cGetUsersWithAcountNumber  = runUsersWActNum
      , cGetUsersWithSSN           = runner "getUsersWithSSN" . findUsersWithSSN
      , cFindToken                 = runFindTok
      , cSaveToken                 = runner "saveToken" . saveToken
      }

  -- Databse Migrations
  withResource psqlPool Migrations.createTokenTable

  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = True
        , settingDisableSession               = False
        , settingUseServerName                = False
        }
  manager <- newManager $ mkManagerSettings tlsSettings Nothing
  aptoEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://apiapto-web-internal.default.svc.cluster.local:443"
  dwollaEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://apidwolla-web-internal.default.svc.cluster.local:443"
  paymentAuthEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://payment-auth-web-internal.default.svc.cluster.local:443"
  privacyEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://api-privacy-web-internal.default.svc.cluster.local:443"

  (ssnKey, ssnContext) <- getSSNSecrets secretGetter
  let vSsnAction   = postEncryption tlsManager vBase vToken (KeyName ssnKey)
  let ssnDecrypter = decrypt vSsnAction (Just ssnContext)
  let ssnEncrypter = encrypt vSsnAction (Just ssnContext)

  let onewaySSNAction =
        postEncryption tlsManager vBase vToken (KeyName "ssn_encryption")
  let oneWaySSNDecrypt = decrypt onewaySSNAction Nothing
  let

  cognitohq <- secretGetter "accounts-fsm/cognitohq"
  let cognitoKey    = BC.pack $ extractSecret cognitohq "key"
  let cognitoSecret = BC.pack $ extractSecret cognitohq "secret"
  let cognitoURL    = extractSecret cognitohq "baseurl"
  cognitoManager <- newCognitoManager cognitoSecret cognitoKey
  cognitoEnv     <- mkClientEnv cognitoManager <$> parseBaseUrl cognitoURL

  segmentSecrets <- secretGetter "tracking"
  let trackingEnv  = extractSecret segmentSecrets "environment"
  let segmentToken = extractSecret segmentSecrets "segment_token"
  sdInfo           <- getStackdriverSecrets secretGetter

  -- force evaluation b/c AppConfig is lazy
  segmentRequester <- case trackingEnv of
    "safari" -> createSegmentRequestor segmentToken
    _        -> do
      let fn _ = do
            putStrLn "Not sending to segment..."
            return ()
      return fn

  let environment = case trackingEnv of
        "safari" -> Production
        _        -> Staging

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

  let appConf = AppConfig { dbActions
                          , aptoEnv
                          , privacyEnv
                          , dwollaEnv
                          , paymentAuthEnv
                          , dbPool                 = psqlPool
                          , segmentRequest         = segmentRequester
                          , environment
                          , stackDriver            = sdInfo
                          , cognitoEnv
                          , convergentSSNDecrypter = ssnDecrypter
                          , convergentSSNEncrypter = ssnEncrypter
                          , onewaySSNDecrypter     = oneWaySSNDecrypt
                          , isShuttingDown         = isShuttingDown
                          , willowEnv              = willowEnv
                          }

  let myQueue = "accounts"
  chan <- startChannel (Just myQueue) amqpString
  _    <- declareExchange
    (getAMQPChan chan)
    newExchange { exchangeName    = "tgthr"
                , exchangeType    = "topic"
                , exchangeDurable = True
                }
  _ <- declareQueue (getAMQPChan chan) newQueue { queueName = myQueue }
  bindQueue (getAMQPChan chan) myQueue "tgthr" "*.event.#"
  bindQueue (getAMQPChan chan) myQueue "tgthr" "account.#"
  _          <- addConsumer myQueue (retryCallback appConf) chan

  certEither <- getPKICert tlsManager
                           vBase
                           vToken
                           "accounts-fsm-web.default.svc.cluster.local"
  VaultCertResponse {..} <- case certEither of
    Left e -> do
      putStr "Error getting certs: " >> print e
      error "Can't get cert"
    Right c -> return c
  putStr "Cert: " >> print serial_number
  let tlsOpts =
        tlsSettingsMemory (encodeUtf8 certificate) (encodeUtf8 private_key)
  let warpOpts = setPort 8080 defaultSettings

  logger <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                       formatAsJSONWithHeaders
    }
  internalAPI <-
    async
    . runTLS tlsOpts warpOpts
    . stackDriverMiddleware sdInfo
    . logger
    . gzip (def { gzipFiles = GzipCompress })
    . webserver
    $ appConf

  link internalAPI

  -- wait for kill signal
  _ <- readMVar shutdownNow

  -- kill
  putStrLn "SIGTERM received... Killing Internal, External APIs"

  cancel internalAPI

  putStrLn "SIGTERM received... All Done"

