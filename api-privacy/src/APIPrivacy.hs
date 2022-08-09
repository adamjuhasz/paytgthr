
{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE RecordWildCards #-}

module APIPrivacy where

import           APIPrivacy.AppMonad            ( APISettings(..) )
import           APIPrivacy.ExternalAPI         ( externalApiApp )
import           APIPrivacy.InternalAPI         ( internalApiApp )
import           APIPrivacy.PrivacyClient       ( newPrivacyManager )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async
                                                , cancel
                                                , link
                                                )
import           Control.Concurrent.MVar        ( newEmptyMVar
                                                , putMVar
                                                , readMVar
                                                )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.Connection             ( TLSSettings(..) )
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , getPort
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
import           Shared.Track.Stackdriver       ( getStackdriverSecrets
                                                , stackDriverMiddleware
                                                )
import           Shared.Vault                   ( KeyName(KeyName)
                                                , VaultCertResponse(..)
                                                , decrypt
                                                , extractSecret
                                                , getPKICert
                                                , getSecretOrThrow
                                                , initManager
                                                , postEncryption
                                                , readTokenFile
                                                )
import           Shared.Vault.SSN               ( getSSNSecrets )
import           Shared.Web.RequestLogger       ( formatAsJSONWithHeaders )
import           System.Posix.Signals           ( Handler(Catch)
                                                , installHandler
                                                , sigTERM
                                                )

data APIPrivacySettings = APIPrivacySettings
  { vBase     :: String
  , tokenFile :: String
  , extPort   :: String
  , intPort   :: String
  }

mainAPIPrivacy :: APIPrivacySettings -> IO ()
mainAPIPrivacy APIPrivacySettings {..} = do
  vToken     <- readTokenFile tokenFile
  tlsManager <- initManager

  let secretGetter = getSecretOrThrow tlsManager vBase vToken

  (ssnKey, ssnContext) <- getSSNSecrets secretGetter

  pinSecret            <- secretGetter "apto-api/pin_transit_key"
  let vPinKey = extractSecret pinSecret "key"

  privacySecret <- secretGetter "privacy-api"
  let privacyApiKey = T.pack $ extractSecret privacySecret "privacy-api-secret"
  let privacyApiUrl = extractSecret privacySecret "privacy-api-url"

  let vSsnAction    = postEncryption tlsManager vBase vToken (KeyName ssnKey)
  let ssnDecrypt    = decrypt vSsnAction (Just ssnContext)
  let vPinAction    = postEncryption tlsManager vBase vToken (KeyName vPinKey)
  let pinDecrypt    = decrypt vPinAction Nothing

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

  sdInfo         <- getStackdriverSecrets secretGetter

  -- pushing data into `toDie` will cause the race to finish
  -- https://stackoverflow.com/a/45846292/369198
  isShuttingDown <- newEmptyMVar
  shutdownNow    <- newEmptyMVar
  let handler = Catch $ do
        putStrLn "SIGTERM received..."
        putMVar isShuttingDown True
        threadDelay 70000000 -- 70 seconds
        putStrLn "SIGTERM received... Shutting down now"
        putMVar shutdownNow True

  _          <- installHandler sigTERM handler Nothing

  certEither <- getPKICert
    tlsManager
    vBase
    vToken
    "api-privacy-web-internal.default.svc.cluster.local"

  VaultCertResponse {..} <- case certEither of
    Left e -> do
      putStr "Error getting certs: " >> print e
      error "Can't get cert"
    Right c -> return c
  putStr "Cert: " >> print serial_number

  let tlsOpts =
        tlsSettingsMemory (encodeUtf8 certificate) (encodeUtf8 private_key)

  let externalOpts = setPort (read extPort) defaultSettings

  privacyManager <- newPrivacyManager
  privacyEnv     <- mkClientEnv privacyManager <$> parseBaseUrl privacyApiUrl

  let apiSettings = APISettings { accountsEnv    = accountsEnv
                                , payAuthEnv     = payAuthEnv
                                , ssnDecrypter   = ssnDecrypt
                                , pinDecrypter   = pinDecrypt
                                , privacyEnv     = privacyEnv
                                , privacyKey     = privacyApiKey
                                , isShuttingDown = isShuttingDown
                                }

  logger <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                       formatAsJSONWithHeaders
    }

  let internalOpts = setPort (read intPort) defaultSettings
  putStr "Starting Internal API on port " >> print (getPort internalOpts)
  internalApi <-
    async
    $ runTLS tlsOpts internalOpts
    $ stackDriverMiddleware sdInfo
    $ logger
    $ gzip (def { gzipFiles = GzipCompress })
    $ internalApiApp apiSettings

  link internalApi

  -- -- --

  -- https://cloud.google.com/load-balancing/docs/ssl-certificates/encryption-to-the-backends
  -- Google does not care about tls cert used
  putStr "Starting External API on port " >> print (getPort externalOpts)
  externalApi <-
    async
    $ runTLS tlsOpts externalOpts
    $ stackDriverMiddleware sdInfo
    $ logger
    $ gzip (def { gzipFiles = GzipCompress })
    $ externalApiApp apiSettings

  link externalApi

  -- wait for kill signal
  _ <- readMVar shutdownNow

  -- kill
  putStrLn "SIGTERM received... Killing Internal, External APIs"

  cancel internalApi
  cancel externalApi

  putStrLn "SIGTERM received... All Done"
