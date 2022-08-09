{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE RecordWildCards, StrictData, NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module LandingPage where

import           Control.Concurrent
import           Data.Aeson.Types               ( Value(String) )
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as BC8
import           Data.Coerce                    ( coerce )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import qualified Data.HashMap.Strict           as HMS
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Database.PostgreSQL.Simple     ( Connection )
import           LandingPage.DB.Migrations      ( runMigration )
import           LandingPage.Linker.LinkerDB    ( retriveLink
                                                , storeLink
                                                )
import           LandingPage.Types              ( AptoSecrets(..)
                                                , ClusterEnvironment(..)
                                                , EncryptedPin(..)
                                                , EncryptedSSN(..)
                                                , EncryptedToken(..)
                                                )
import           LandingPage.Web               as Web
                                                ( AppSettings(..)
                                                , startServer
                                                )
import           Shared.Amqp                    ( startChannel )
import           Shared.Database                ( createDBPool )
import           Shared.Vault                   ( CipherText(CipherText)
                                                , KeyName(KeyName)
                                                , VaultCredResponse(..)
                                                , decrypt
                                                , encrypt
                                                , extractSecret
                                                , getRMQCreds
                                                , getSecretOrThrow
                                                , initManager
                                                , postEncryption
                                                , readTokenFile
                                                )
import           Shared.Vault.SSN               ( getSSNSecrets )
import           System.Posix.Signals
import qualified Web.ClientSession             as WCS
import           Web.Hashids                    ( createHashidsContext )

import           Shared.Track.Stackdriver

data LandingPageSettings = LandingPageSettings
  { rmqRole     :: String
  , rmqAddr     :: String
  , vBase       :: String
  , tokenFile   :: String
  , publicLoc   :: Maybe String
  , templateLoc :: Maybe String
  }

mainLandingPage :: LandingPageSettings -> IO ()
mainLandingPage LandingPageSettings {..} = do
  vToken     <- readTokenFile tokenFile
  tlsManager <- initManager

  let secretGetter = getSecretOrThrow tlsManager vBase vToken
  (ssnKey, ssnContext) <- getSSNSecrets secretGetter

  (rmqU  , rmpP      ) <- getRMQCreds tlsManager vBase vToken rmqRole <&> \case
    Left  e                      -> error e
    Right VaultCredResponse {..} -> (unpack vcUsername, unpack vcPassword)

  putStr "RMQ Username: " >> print rmqU
  let amqpString = "amqp://" <> rmqU <> ":" <> rmpP <> "@" <> rmqAddr

  chan              <- startChannel (Just "landing-page") amqpString <&> Just

  secrets           <- secretGetter "landing-page/secrets"
  sessionSecrets    <- secretGetter "landing-page/session"
  pinSecrets        <- secretGetter "landing-page/pin_transit_key"
  plaidSecrets      <- secretGetter "landing-page/plaid"
  passwordSecrets   <- secretGetter "landing-page/password_reset_key"
  aptoProxySecrets  <- secretGetter "landing-page/apto"

  sdInfo            <- getStackdriverSecrets secretGetter

  let psqlString    = extractSecret secrets "psql"
  let hashidsSalt   = extractSecret secrets "hashidSalt"
  let hashidsMinLen = read $ extractSecret secrets "hashidMinLen"
  let domain = case secrets HMS.! "host" of
        String t -> t
        _        -> error "bad value for host"
  let key       = extractSecret sessionSecrets "session_key"
  let vPinKey   = extractSecret pinSecrets "key"
  let plaidEnv  = extractSecret plaidSecrets "environment"
  let vTokenKey = extractSecret passwordSecrets "key"
  let aptoBase  = extractSecret aptoProxySecrets "base_url"
  let aptoUser  = BC8.pack $ extractSecret aptoProxySecrets "username"
  let aptoPass  = BC8.pack $ extractSecret aptoProxySecrets "password"
  let environment = case extractSecret secrets "environment" of
        "production" -> ProductionEnv
        "staging"    -> StagingEnv
        _            -> DevelopmentEnv

  let vSsnAction       = postEncryption tlsManager vBase vToken (KeyName ssnKey)
  let vPinAction = postEncryption tlsManager vBase vToken (KeyName vPinKey)
  let vTokenAction = postEncryption tlsManager vBase vToken (KeyName vTokenKey)
  let aptoSecrets      = AptoSecrets { aptoBase, aptoUser, aptoPass }

  psqlPool <- createDBPool psqlString
  let poolRun :: (Connection -> IO a) -> IO a
      poolRun fn = withResource psqlPool $ \conn -> fn conn
  let hashIdscontext = createHashidsContext (BC8.pack hashidsSalt)
                                            hashidsMinLen
                                            "cdfhjkmnprtvwxy2345689"

  poolRun runMigration

  isShuttingDown :: MVar Bool <- newEmptyMVar
  shutdownNow :: MVar Bool    <- newEmptyMVar
  let handler = Catch $ do
        putStrLn "SIGTERM received..."
        putMVar isShuttingDown True
        threadDelay 70000000 -- 70 seconds
        putStrLn "SIGTERM received... Shutting down now"
        putMVar shutdownNow True
  _ <- installHandler sigTERM handler Nothing

  let tgthrSettings = AppSettings
        { environment      = environment
        , amqpChannel      = chan
        , sessionKey       = makeKey key
        , port             = 3000
        , ssnEncrypter     = fmap coerce . encrypt vSsnAction (Just ssnContext)
        , pinEncrypter     = fmap coerce . encrypt vPinAction Nothing
        , plaidEnvironment = pack plaidEnv
        , tokenEncrypter   = fmap coerce . encrypt vTokenAction Nothing
        , tokenDecrypter   = decrypt vTokenAction Nothing . coerce
        , publicDir        = fromMaybe "public" publicLoc
        , templateDir      = fromMaybe "templates" templateLoc
        , linkGet          = poolRun . retriveLink hashidsMinLen hashIdscontext
        , linkPut          = poolRun . storeLink hashIdscontext
        , domain           = domain
        , aptoSecrets
        , withDBPool       = poolRun
        , stackDriverInfo  = Just sdInfo
        , goingToShutdown  = isShuttingDown
        }
  Web.startServer tgthrSettings
  return ()
 where
  fromRight a = case a of
    Left  _ -> Prelude.error "Left not allowed"
    Right x -> x
  makeKey s = s & BC8.pack & B64.decode & fromRight & WCS.initKey & fromRight
