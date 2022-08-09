{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           APIPrivacy                     ( APIPrivacySettings(..)
                                                , mainAPIPrivacy
                                                )
import           LoadEnv                        ( loadEnv )
import           System.Environment             ( getEnv )
import           System.IO                      ( BufferMode(LineBuffering)
                                                , hSetBuffering
                                                , stderr
                                                , stdout
                                                )

main :: IO ()
main = do
  putStr "Starting api-privacy..."
  _          <- loadEnv
  _          <- hSetBuffering stdout LineBuffering
  _          <- hSetBuffering stderr LineBuffering

  extPort    <- getEnv "EXT_PORT"
  intPort    <- getEnv "INT_PORT"
  vBase      <- getEnv "VAULT_BASE"
  tokenFile  <- getEnv "TOKEN_FILE_LOCATION"

  let settings = APIPrivacySettings { .. }
  mainAPIPrivacy settings

