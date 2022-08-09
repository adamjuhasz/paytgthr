{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           LoadEnv                        ( loadEnv )
import           System.Environment             ( getEnv )
import           System.IO
import           APIDwolla

main :: IO ()
main = do
  _           <- loadEnv
  _           <- hSetBuffering stdout LineBuffering
  _           <- hSetBuffering stderr LineBuffering

  desiredPort <- getEnv "PORT"
  vBase       <- getEnv "VAULT_BASE"
  tokenFile   <- getEnv "TOKEN_FILE_LOCATION"
  rmqRole     <- getEnv "RMQ_ROLE"
  rmqAddr     <- getEnv "AMQP_BASE"

  let settings =
        APIDwollaSettings { vBase, tokenFile, rmqRole, rmqAddr, desiredPort }
  mainAPIDwolla settings
