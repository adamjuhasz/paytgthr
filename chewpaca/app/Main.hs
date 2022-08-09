{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           LoadEnv                        ( loadEnv )
import           System.Environment             ( getEnv )
import           System.IO
import           Chewpaca

main :: IO ()
main = do
  _           <- loadEnv
  _           <- hSetBuffering stdout LineBuffering
  _           <- hSetBuffering stderr LineBuffering
  desiredPort <- getEnv "PORT"
  rmqRole     <- getEnv "RMQ_ROLE"
  rmqAddr     <- getEnv "AMQP_BASE"
  vBase       <- getEnv "VAULT_BASE"
  tokenFile   <- getEnv "TOKEN_FILE_LOCATION"

  let settings = ChewpacaSettings { desiredPort
                                  , rmqRole
                                  , rmqAddr
                                  , vBase
                                  , tokenFile
                                  , publicLoc   = Nothing
                                  }
  mainChewpaca settings
