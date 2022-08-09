{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE NamedFieldPuns #-}

module Main where


import           LoadEnv                        ( loadEnv )
import           System.Environment             ( getEnv )
import           System.IO
import           PaymentAuth

main :: IO ()
main = do
  loadEnv
  _           <- hSetBuffering stdout LineBuffering
  _           <- hSetBuffering stderr LineBuffering

  desiredPort <- getEnv "PORT"
  rmqRole     <- getEnv "RMQ_ROLE"
  rmqAddr     <- getEnv "AMQP_BASE"
  vBase       <- getEnv "VAULT_BASE"
  tokenFile   <- getEnv "TOKEN_FILE_LOCATION"
  ekgPort     <- getEnv "EKG_PORT"

  let settings = PaymentAuthSettings { desiredPort
                                     , rmqRole
                                     , rmqAddr
                                     , vBase
                                     , tokenFile
                                     , ekgPort
                                     }
  mainPaymentAuth settings


