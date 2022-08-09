{-# LANGUAGE RecordWildCards #-}


module APIDwolla where

import           APIDwolla.AppMonad             ( InternalAPISettings(..) )
import           APIDwolla.Dwolla.Client        ( createAccessToken
                                                , createMananger
                                                )
import           APIDwolla.ExternalAPI          ( AppSettings(..)
                                                , apiApp
                                                )
import           APIDwolla.InternalAPI          ( internalApiApp )
import           Control.Concurrent             ( newEmptyMVar
                                                , putMVar
                                                , readMVar
                                                , threadDelay
                                                )
import           Control.Concurrent.Async       ( async
                                                , cancel
                                                , link
                                                )
import qualified Data.ByteString.Char8         as BC8
import           Data.Function                  ( (&) )
import           Data.Text.Encoding             ( encodeUtf8 )
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
import           Servant.Client                 ( mkClientEnv
                                                , parseBaseUrl
                                                )
import           Shared.Track.Stackdriver       ( getStackdriverSecrets
                                                , stackDriverMiddleware
                                                )
import           Shared.Vault                   ( VaultCertResponse(..)
                                                , extractSecret
                                                , getPKICert
                                                , getSecretOrThrow
                                                , initManager
                                                , readTokenFile
                                                )
import           Shared.Web.RequestLogger       ( formatAsJSONWithHeaders )
import           Shared.Web.Utils               ( warpDefaultSettings )
import           System.Posix.Signals           ( Handler(Catch)
                                                , installHandler
                                                , sigTERM
                                                )

data APIDwollaSettings = APIDwollaSettings
  { vBase       :: String
  , tokenFile   :: String
  , rmqRole     :: String
  , rmqAddr     :: String
  , desiredPort :: String
  }

mainAPIDwolla :: APIDwollaSettings -> IO ()
mainAPIDwolla APIDwollaSettings {..} = do
  vToken     <- readTokenFile tokenFile
  tlsManager <- initManager

  let secretGetter = getSecretOrThrow tlsManager vBase vToken
  aptoTokenSecrets <- secretGetter "dwolla-api/dwolla-token"
  let dwollaBase = extractSecret aptoTokenSecrets "base"
      dwollaUser = extractSecret aptoTokenSecrets "client_id"
      dwollaPass = extractSecret aptoTokenSecrets "client_secret"

  manager <- createMananger
  let tokenMaker = createAccessToken dwollaBase
                                     (BC8.pack dwollaUser)
                                     (BC8.pack dwollaPass)
                                     manager

  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = True
        , settingDisableSession               = False
        , settingUseServerName                = False
        }
  internalManager <- newManager $ mkManagerSettings tlsSettings Nothing
  accountsEnv     <- mkClientEnv internalManager
    <$> parseBaseUrl "https://accounts-fsm-web.default.svc.cluster.local:443"
  paymentsEnv <- mkClientEnv internalManager <$> parseBaseUrl
    "https://payment-auth-web-internal.default.svc.cluster.local:443"

  sdInfo     <- getStackdriverSecrets secretGetter

  jsonLogger <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                       formatAsJSONWithHeaders
    }

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

  let settings = AppSettings { inProd         = True
                             , port           = read desiredPort
                             , tokener        = tokenMaker
                             , payAuthEnv     = paymentsEnv
                             , accountsEnv    = accountsEnv
                             , isShuttingDown = isShuttingDown
                             }

  let warpSettings = warpDefaultSettings & setPort (read desiredPort)
  putStr "Starting External API on port " >> print (getPort warpSettings)
  externalApp <- apiApp settings
  externalAPI <-
    async
    . runSettings warpSettings
    . stackDriverMiddleware sdInfo
    . jsonLogger
    $ externalApp

  certEither <- getPKICert tlsManager
                           vBase
                           vToken
                           "apidwolla-web-internal.default.svc.cluster.local"
  VaultCertResponse {..} <- case certEither of
    Left e -> do
      putStr "Error getting certs: " >> print e
      error "Can't get cert"
    Right c -> return c
  putStr "Cert: " >> print serial_number
  let tlsOpts =
        tlsSettingsMemory (encodeUtf8 certificate) (encodeUtf8 private_key)
  let warpOpts = setPort 8443 defaultSettings

  internalAPI <-
    async
    . runTLS tlsOpts warpOpts
    . stackDriverMiddleware sdInfo
    . jsonLogger
    . gzip (def { gzipFiles = GzipCompress })
    . internalApiApp
    $ InternalAPISettings { tokenMaker     = tokenMaker
                          , accountsEnv    = accountsEnv
                          , paymentsEnv    = paymentsEnv
                          , isShuttingDown = isShuttingDown
                          }

  link internalAPI
  link externalAPI

  -- wait for kill signal
  _ <- readMVar shutdownNow

  -- kill
  putStrLn "SIGTERM received... Killing Internal, External APIs"

  cancel internalAPI

  putStrLn "SIGTERM received... All Done"

