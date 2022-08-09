{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}

module Main where

import           AccountsFSM
import           APIApto
import           APIDwolla
import           Auditor
import           Chewpaca
import           LandingPage
import           Mailer
import           PaymentAuth
import           Control.Concurrent
import           Control.Monad
import           LoadEnv                        ( loadEnv )
import           System.Environment             ( getEnv )
import           System.IO

main :: IO ()
main = do
  _         <- loadEnv
  _         <- hSetBuffering stdout LineBuffering
  _         <- hSetBuffering stderr LineBuffering

  rmqRole   <- getEnv "RMQ_ROLE"
  rmqAddr   <- getEnv "AMQP_BASE"
  vBase     <- getEnv "VAULT_BASE"
  tokenFile <- getEnv "TOKEN_FILE_LOCATION"

  let
    accountSettings =
      AccountsFSMSettings { rmqRole, rmqAddr, vBase, tokenFile }
    apiAptiSettings = APIAptoSettings { vBase
                                      , tokenFile
                                      , rmqRole
                                      , rmqAddr
                                      , desiredPort = "4002"
                                      }
    apiDwollaSettings = APIDwollaSettings { vBase
                                          , tokenFile
                                          , rmqRole
                                          , rmqAddr
                                          , desiredPort = "4003"
                                          }
    auditorSettings  = AuditorSettings { rmqRole, rmqAddr, vBase, tokenFile }
    chewpacaSettings = ChewpacaSettings { desiredPort = "4000"
                                        , rmqRole
                                        , rmqAddr
                                        , vBase
                                        , tokenFile
                                        , publicLoc = Just "../chewpaca/public"
                                        }
    landingPageSettings = LandingPageSettings
      { rmqRole
      , rmqAddr
      , vBase
      , tokenFile
      , publicLoc   = Just "../landing-page/public"
      , templateLoc = Just "../landing-page/templates"
      }
    mailerSettings      = MailerSettings { vBase, tokenFile, rmqRole, rmqAddr }
    paymentAuthSettings = PaymentAuthSettings { desiredPort = "4001"
                                              , rmqRole
                                              , rmqAddr
                                              , vBase
                                              , tokenFile
                                              , ekgPort     = "8001"
                                              }

  _ <- forkIO $ mainAccountsFSM accountSettings
  _ <- forkIO $ mainAPIApto apiAptiSettings
  _ <- forkIO $ mainAPIDwolla apiDwollaSettings
  _ <- forkIO $ mainAuditor auditorSettings
  _ <- forkIO $ mainChewpaca chewpacaSettings
  _ <- forkIO $ mainLandingPage landingPageSettings
  _ <- forkIO $ mainMailer mailerSettings
  _ <- forkIO $ mainPaymentAuth paymentAuthSettings

  forever $ threadDelay 100000
