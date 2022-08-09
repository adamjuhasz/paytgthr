{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: Get all events and log them places
Description: Log them all
Copyright: (c) Pay Tgthr

Too long should be broken up
-}
module Auditor where

import           Auditor.DB                     ( insertMessage )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Data.Functor                   ( (<&>) )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( unpack )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Network.AMQP                   ( ExchangeOpts(..)
                                                , QueueOpts(queueName)
                                                , bindQueue
                                                , declareExchange
                                                , declareQueue
                                                , newExchange
                                                , newQueue
                                                )
import           Shared.Amqp                    ( AMQPCallback
                                                , addConsumer
                                                , getAMQPChan
                                                , startChannel
                                                )
import           Shared.Database                ( PooledDB
                                                , createDBPool
                                                )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Vault                   ( VaultCredResponse(..)
                                                , extractSecret
                                                , getRMQCreds
                                                , getSecretOrThrow
                                                , initManager
                                                , readTokenFile
                                                )
import           Text.Printf                    ( printf )

timeFormatter :: UTCTime -> String
timeFormatter = formatTime defaultTimeLocale "%x"

showCurr :: Currency -> String
showCurr (Currency _ val) =
  let f :: Double = fromRational val
  in  if fromIntegral (floor f :: Int) == f
        then printf "$%.0f" f
        else printf "$%.2f" f

data AuditorSettings = AuditorSettings
  { rmqRole   :: String
  , rmqAddr   :: String
  , vBase     :: String
  , tokenFile :: String
  }

mainAuditor :: AuditorSettings -> IO ()
mainAuditor AuditorSettings {..} = do
  vToken       <- readTokenFile tokenFile
  tlsManager   <- initManager

  (rmqU, rmpP) <- getRMQCreds tlsManager vBase vToken rmqRole <&> \c ->
    case c of
      Left  e                      -> error e
      Right VaultCredResponse {..} -> (unpack vcUsername, unpack vcPassword)
  let secretGetter = getSecretOrThrow tlsManager vBase vToken
  cdbSecrets <- secretGetter "auditor/cockroach"
  let psqlString = extractSecret cdbSecrets "psql_string"

  putStr "RMQ Username: " >> print rmqU
  let amqpString = "amqp://" <> rmqU <> ":" <> rmpP <> "@" <> rmqAddr

  psqlPool <- createDBPool psqlString
  let cb      = callback psqlPool
  let myQueue = "auditlog"
  chan <- startChannel (Just myQueue) amqpString
  declareExchange
    (getAMQPChan chan)
    newExchange { exchangeName    = "tgthr"
                , exchangeType    = "topic"
                , exchangeDurable = True
                }
  _ <- declareQueue (getAMQPChan chan) newQueue { queueName = myQueue }
  bindQueue (getAMQPChan chan) myQueue "tgthr" "#"
  _ <- addConsumer myQueue cb chan
  forever $ threadDelay 100000

callback :: PooledDB -> AMQPCallback
callback pool _ mid msg = do
  putStr "Processing " <> print mid
  withResource pool (insertMessage msg)
  return (Nothing, [])
