{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.Ledger.CreateJournal where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Ledger.CreateJournal
                                                ( checkSingleton
                                                , createNewJournal
                                                )
import           PaymentAuth.Monad.Ledger       ( CreateLedgerJournalBody(..)
                                                , HasLedgerDB(..)
                                                )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Servant                        ( NoContent(..) )
import           Servant.Server                 ( ServerError(errBody)
                                                , err400
                                                )
import           Shared.Console                 ( tracePrint )
import           Shared.WebAPI.General.API      ( TraceContext )

createJournal
  :: ( HasLedgerDB m
     , HasTime m
     , HasRandom m
     , MonadError ServerError m
     , MonadIO m
     )
  => TraceContext
  -> CreateLedgerJournalBody
  -> m NoContent
createJournal trace body@CreateLedgerJournalBody {..} = do
  tracePrint trace "createJournal " body

  hasSingletonAlready <- checkSingleton trace newJournalType
  when hasSingletonAlready
    $ throwError err400 { errBody = "Journal Type already exists for user" }

  createNewJournal trace body

  return NoContent
