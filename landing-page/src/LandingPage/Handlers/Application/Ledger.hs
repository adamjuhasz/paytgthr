{-# LANGUAGE RecordWildCards #-}
module LandingPage.Handlers.Application.Ledger where

import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(Null)
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Time.Clock                ( getCurrentTime )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUserNotClosedHTTP
                                                )
import           Network.HTTP.Types             ( status500 )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Ledger.Journal   ( JournalSearch(GetPayTgthr)
                                                , LedgerJournal(..)
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( Routes(..)
                                                , payAuthClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                , status
                                                )

type AccountsEnv = ClientEnv
type PayAuthEnv = ClientEnv

getLedger :: V.Key SessionData -> AccountsEnv -> PayAuthEnv -> ActionM ()
getLedger sKey accountsEnv payAuthEnv = do
  (uid, _) <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosedHTTP accountsEnv

  trace <- createTrace

  let fn = _GetJournal payAuthClientM trace $ GetPayTgthr uid
  now <- liftAndCatchIO getCurrentTime

  res <- liftAndCatchIO $ runClientM fn payAuthEnv
  case res of
    Right (LedgerJournal {..} : _) -> Scotty.json $ object
      [ "balance" .= journalBalance
      , "revision" .= journalRevision
      , "updated" .= journalUpdated
      ]
    Right [] -> Scotty.json $ object
      [ "balance" .= Currency "USD" 0
      , "revision" .= (0 :: Int)
      , "updated" .= now
      ]
    Left e -> do
      liftAndCatchIO $ putStr "Error: getLedger _GetLedger " >> print
        (uid, trace, e)
      status status500 >> Scotty.json Null
