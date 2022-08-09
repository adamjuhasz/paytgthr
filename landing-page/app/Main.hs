{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           System.Environment             ( getEnv )
import           System.IO
import           LoadEnv
import           LandingPage

main :: IO ()
main = do
  loadEnv
  _         <- hSetBuffering stdout LineBuffering
  _         <- hSetBuffering stderr LineBuffering

  rmqRole   <- getEnv "RMQ_ROLE"
  rmqAddr   <- getEnv "AMQP_BASE"
  vBase     <- getEnv "VAULT_BASE"
  tokenFile <- getEnv "TOKEN_FILE_LOCATION"

  let settings = LandingPageSettings { rmqRole
                                     , rmqAddr
                                     , vBase
                                     , tokenFile
                                     , publicLoc   = Nothing
                                     , templateLoc = Nothing
                                     }
  mainLandingPage settings
