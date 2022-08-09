{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE RecordWildCards #-}

module APIApto where

import           APIApto.AppMonad               ( IntAPISettings(..)
                                                , PinDecrypter
                                                , SSNDecrypter
                                                )
import           APIApto.Apto.Client            ( AptoSecrets(..)
                                                , RequesterWithID
                                                , createMananger
                                                , generateRequest
                                                )
import           APIApto.Callback               ( callback )
import           APIApto.ExternalAPI            ( ExtApiSettings(..)
                                                , externalApiApp
                                                )
import           APIApto.InternalAPI            ( internalApiApp )
import           Control.Concurrent.Async       ( async
                                                , cancel
                                                , link
                                                )
import           Control.Concurrent.MVar        ( newEmptyMVar
                                                , readMVar
                                                )
import           Control.Retry                  ( constantDelay
                                                , limitRetries
                                                , recoverAll
                                                )
import qualified Data.ByteString.Char8         as BC8
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
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
import           Shared.Web.RequestLogger
                                                ( formatAsJSONWithHeaders )
import           Servant.Client                 ( ClientEnv
                                                , mkClientEnv
                                                , parseBaseUrl
                                                , runClientM
                                                )
import           Shared.Amqp                    ( AMQPCallback
                                                , addConsumer
                                                , getAMQPChan
                                                , retryAMQPCallbackManualTimeout
                                                , startChannel
                                                )
import           Shared.Vault                   ( KeyName(KeyName)
                                                , VaultCertResponse(..)
                                                , VaultCredResponse(..)
                                                , decrypt
                                                , extractSecret
                                                , getPKICert
                                                , getRMQCreds
                                                , getSecretOrThrow
                                                , initManager
                                                , postEncryption
                                                , readTokenFile
                                                )
import           Shared.Vault.SSN               ( getSSNSecrets )
import           Shared.Web.Utils               ( warpDefaultSettings )
import qualified Shared.WebAPI.AccountsFSM.Client
                                               as Accounts

data APIAptoSettings = APIAptoSettings
  { vBase       :: String
  , tokenFile   :: String
  , rmqRole     :: String
  , rmqAddr     :: String
  , desiredPort :: String
  }

-- brittany-next-binding --columns 100
retryCallback :: SSNDecrypter -> PinDecrypter -> RequesterWithID -> ClientEnv -> AMQPCallback
retryCallback a b c d e f g =
  recoverAll (constantDelay 50000 <> limitRetries 2)
    $ retryAMQPCallbackManualTimeout "APIApto" 30000000 (callback a b c d) e f g

mainAPIApto :: APIAptoSettings -> IO ()
mainAPIApto APIAptoSettings {..} = do
  vToken     <- readTokenFile tokenFile

  jsonLogger <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                       formatAsJSONWithHeaders
    }

  tlsManager   <- initManager
  (rmqU, rmpP) <- getRMQCreds tlsManager vBase vToken rmqRole <&> \c ->
    case c of
      Left e -> error $ "Error: getRMQCreds err " <> show e
      Right VaultCredResponse {..} -> (unpack vcUsername, unpack vcPassword)

  let secretGetter = getSecretOrThrow tlsManager vBase vToken
  (ssnKey, ssnContext) <- getSSNSecrets secretGetter
  aptoTokenSecrets     <- secretGetter "apto-api/apto_token"
  ourTokenSecrets      <- secretGetter "apto-api/our_token"
  pinSecret            <- secretGetter "apto-api/pin_transit_key"
  let aptoBase     = extractSecret aptoTokenSecrets "base_url"
      aptoUser     = extractSecret aptoTokenSecrets "username"
      aptoPassword = extractSecret aptoTokenSecrets "password"
      authUsername = extractSecret ourTokenSecrets "username"
      authPassword = extractSecret ourTokenSecrets "password"
      vPinKey      = extractSecret pinSecret "key"


  putStr "RMQ Username: " >> print rmqU
  let amqpString = "amqp://" <> rmqU <> ":" <> rmpP <> "@" <> rmqAddr
  let vSsnAction = postEncryption tlsManager vBase vToken (KeyName ssnKey)
      ssnDecrypt = decrypt vSsnAction (Just ssnContext)
  let vPinAction = postEncryption tlsManager vBase vToken (KeyName vPinKey)
      pinDecrypt = decrypt vPinAction Nothing

  let aptoSecret = AptoSecrets { baseURL  = aptoBase
                               , username = BC8.pack aptoUser
                               , password = BC8.pack aptoPassword
                               }
  client <- generateRequest aptoSecret . fst <$> createMananger aptoSecret

  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = True
        , settingDisableSession               = False
        , settingUseServerName                = False
        }
  manager     <- newManager $ mkManagerSettings tlsSettings Nothing
  accountsEnv <- mkClientEnv manager
    <$> parseBaseUrl "https://accounts-fsm-web.default.svc.cluster.local:443"
  payAuthEnv <- mkClientEnv manager <$> parseBaseUrl
    "https://payment-auth-web-internal.default.svc.cluster.local:443"

  let myQueue = "apiapto"
  chan <- startChannel (Just myQueue) amqpString
  declareExchange
    (getAMQPChan chan)
    newExchange { exchangeName    = "tgthr"
                , exchangeType    = "topic"
                , exchangeDurable = True
                }
  _ <- declareQueue (getAMQPChan chan) newQueue { queueName = myQueue }
  bindQueue (getAMQPChan chan) myQueue "tgthr" "account.event.#"
  bindQueue (getAMQPChan chan) myQueue "tgthr" "apto.cmd.#"
  _ <- addConsumer myQueue
                   (retryCallback ssnDecrypt pinDecrypt client accountsEnv)
                   chan

  toDie <- newEmptyMVar
  let settings = ExtApiSettings
        { inProd         = True
        , port           = read desiredPort
        , authSettings   = (BC8.pack authUsername, BC8.pack authPassword)
        , aptoClient     = client
        , toDie          = toDie
        , accountsEnv    = accountsEnv
        , paymentAuthEnv = payAuthEnv
        }
  let warpSettings = warpDefaultSettings & setPort (read desiredPort)

  -- pushing data into `toDie` will cause the race to finish
  -- https://stackoverflow.com/a/45846292/369198
  putStr "Starting External API on port " >> print (getPort warpSettings)
  externalWarpApp <- externalApiApp settings
  externalApp <- async $ runSettings warpSettings $ jsonLogger externalWarpApp

  certEither <- getPKICert tlsManager
                           vBase
                           vToken
                           "apiapto-web-internal.default.svc.cluster.local"
  VaultCertResponse {..} <- case certEither of
    Left e -> do
      putStr "Error getting certs: " >> print e
      error "Can't get cert"
    Right c -> return c
  putStr "Cert: " >> print serial_number
  let tlsOpts =
        tlsSettingsMemory (encodeUtf8 certificate) (encodeUtf8 private_key)
  let warpOpts = setPort 8443 defaultSettings

  putStr "Starting Internal API on port " >> print (getPort warpOpts)

  -- verify a connection to all required services
  -- disable for dev env
  acctPong <- runClientM (Accounts._health Accounts.accountsClientM) accountsEnv
  case acctPong of
    Right "AFSM" -> putStrLn "AFSM connection is good"
    e            -> putStr "Error Account ping: " >> print e

  internalApi <-
    async
    $ runTLS tlsOpts warpOpts
    $ jsonLogger
    $ gzip (def { gzipFiles = GzipCompress })
    $ internalApiApp
    $ IntAPISettings { aptoRequester = client
                     , ssnDecrypter  = ssnDecrypt
                     , pinDecrypter  = pinDecrypt
                     , accountsEnv   = accountsEnv
                     }

  link externalApp
  link internalApi

  -- wait for kill signal
  readMVar toDie

  -- kill
  putStrLn "Error: Killing Internal and External APIs"
  cancel externalApp
  cancel internalApi
