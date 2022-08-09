{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIDwolla.InternalAPI.FsRemove where

import           APIDwolla.Monad.Accounts       ( HasAccounts(..) )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8    as C
import           Servant                        ( NoContent(..)
                                                , ServerError(errBody)
                                                , err500
                                                )
import           Shared.Console
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.ApiDwolla.API    ( RemoveFSBody(..) )
import           Shared.WebAPI.General.API      ( TraceContext )

removefundingSource
  :: (HasAccounts m, HasDwollaClient m, MonadIO m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> RemoveFSBody
  -> m NoContent
removefundingSource trace userId RemoveFSBody {..} = do
  tracePrint trace
             "APIDwolla.InternalAPI.FsRemove.removefundingSource "
             (userId, fsId)

  res <- removeBankAccount trace fsId
  case res of
    Right _ -> return NoContent
    Left  e -> do
      traceError trace
                 "Error: removefundingSource removeBankAccount "
                 (userId, fsId, e)
      throwError err500 { errBody = C.pack $ show (userId, fsId, e) }
