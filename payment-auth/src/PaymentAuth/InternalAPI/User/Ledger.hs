{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.User.Ledger where

import           Control.Exception              ( SomeException )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadMask
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8    as C
import qualified Data.Text                     as T
import           PaymentAuth.App.Ledger.Update  ( createLedgerTransaction )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Servant                        ( NoContent(..) )
import           Servant.Server                 ( ServerError(errBody)
                                                , err500
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.WebAPI.PaymentAuth.API  ( CreateLedgerTrxBody(..)
                                                , TraceContext
                                                )

adjustUsersLedger
  :: ( HasLedgerDB m
     , HasRandom m
     , HasTime m
     , MonadIO m
     , MonadError ServerError m
     , MonadMask m
     )
  => TraceContext
  -> CreateLedgerTrxBody
  -> m NoContent
adjustUsersLedger trace body@CreateLedgerTrxBody {..} = do
  tracePrint trace "adjustUsersLedger " body

  let idem = Just $ "adjustUsersLedger " <> T.pack (show trace)
  let fn   = createLedgerTransaction trace idem fromJournal toJournal fact
  let catcher (e :: SomeException) = do
        traceError trace "Error: adjustUsersLedger " (body, e)
        throwError err500 { errBody = C.pack $ show (e, body) }

  let caught = fn `catch` catcher
  _ <- caught

  return NoContent
