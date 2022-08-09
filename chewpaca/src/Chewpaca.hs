{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE RecordWildCards #-}
module Chewpaca where

import           Chewpaca.Web                   ( AppSettings(..)
                                                , startServer
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( unpack )
import           Shared.Amqp                    ( startChannel )
import           Shared.Database                ( createDBPool )
import           Shared.Track.Stackdriver
import           Shared.Vault                   ( KeyName(..)
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

data ChewpacaSettings = ChewpacaSettings
  { desiredPort :: String
  , rmqRole     :: String
  , rmqAddr     :: String
  , vBase       :: String
  , tokenFile   :: String
  , publicLoc   :: Maybe String
  }

mainChewpaca :: ChewpacaSettings -> IO ()
mainChewpaca ChewpacaSettings {..} = do
  vToken       <- readTokenFile tokenFile
  tlsManager   <- initManager
  (rmqU, rmpP) <- getRMQCreds tlsManager vBase vToken rmqRole <&> \case
    Left  e                      -> error e
    Right VaultCredResponse {..} -> (unpack vcUsername, unpack vcPassword)

  let secretGetter = getSecretOrThrow tlsManager vBase vToken

  (ssnKey, ssnContext) <- getSSNSecrets secretGetter
  let vSsnAction = postEncryption tlsManager vBase vToken (KeyName ssnKey)
  let ssnDecrypt = decrypt vSsnAction (Just ssnContext)
  let ssnEncrypt = encrypt vSsnAction (Just ssnContext)

  cdbSecrets <- secretGetter "chewpaca/cockroach"
  let psqlString = extractSecret cdbSecrets "psql_string"

  putStr "RMQ Username: " >> print rmqU
  let amqpString = "amqp://" <> rmqU <> ":" <> rmpP <> "@" <> rmqAddr

  pool   <- createDBPool psqlString
  chan   <- startChannel (Just "payment-auth") amqpString

  sdInfo <- getStackdriverSecrets secretGetter

  startServer $ AppSettings { inProd          = True
                            , dbPool          = pool
                            , port            = read desiredPort
                            , mqChannel       = Just chan
                            , publicDir       = fromMaybe "public" publicLoc
                            , ssnDecrypt      = ssnDecrypt
                            , ssnEncrypt      = ssnEncrypt
                            , stackDriverInfo = sdInfo
                            }
