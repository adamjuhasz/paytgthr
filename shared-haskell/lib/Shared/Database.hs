{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Database where

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Concurrent.MVar        ( MVar
                                                , newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Exception              ( Handler(Handler)
                                                , SomeException
                                                , catches
                                                )
import           Control.Monad                  ( forever )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC8
import           Data.Pool                      ( Pool
                                                , createPool
                                                , destroyAllResources
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( nominalDay )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only
                                                , SqlError(..)
                                                , close
                                                , connectPostgreSQL
                                                , query_
                                                )

type PooledDB = Pool OpaqueDB
type OpaqueDB = Connection
type Action b a = (Connection -> b -> IO a) -> b -> IO a

connectToDB :: MVar Bool -> B.ByteString -> IO Connection
connectToDB connectionError conn_string = do
  putStrLn "Connecting to Postgres..."
  conn <- connectPostgreSQL conn_string

  -- keep checking if connection has died
  let catcher f = catches
        f
        [ Handler
          (\e@SqlError {..} -> case sqlErrorMsg of
            "connection disconnected" -> return () -- just die
            _                         -> do
              putStr "Error: DB KeepAlive failed " >> print e
              putMVar connectionError True
          )
        , Handler
          (\(e :: SomeException) -> do
            putStr "Error: DB KeepAlive failed (SomeException) " >> print e
            putMVar connectionError True
          )
        ]
  _ <- forkIO . catcher . forever $ do
    _ids :: [Only Text] <- query_ conn "SHOW session_id;"
    threadDelay 30000000 -- X seconds for life check
  return conn

closeDB :: Connection -> IO ()
closeDB conn = do
  putStrLn "Closing Postgress connection..."
  close conn

createDBPool :: String -> IO PooledDB
createDBPool sql_conn = do
  let conn_string = BC8.pack sql_conn
      subPools    = 1
      idleTimeSec = nominalDay
      maxCount    = 30
  connectionError <- newEmptyMVar
  pool            <- createPool (connectToDB connectionError conn_string)
                                closeDB
                                subPools
                                idleTimeSec
                                maxCount
  _ <- forkIO . forever $ do
    _ <- takeMVar connectionError
    putStrLn "Error: Detroying all connections in DB Pool"
    destroyAllResources pool
  putStrLn "Created PSql Pool..."
  return pool
