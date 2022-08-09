{-# LANGUAGE RecordWildCards #-}

module Chewpaca.Web.Handlers.GetCardLastFour where

import           Chewpaca.Web.Handlers.Utils    ( aptoAPIUrl
                                                , genTrace
                                                )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Network.HTTP.Client            ( Manager )
import           Servant.Client                 ( mkClientEnv
                                                , parseBaseUrl
                                                )
import           Shared.Models.Card             ( CardLastFour )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.ApiApto.API      ( Routes(..)
                                                , TraceContext(..)
                                                )
import           Shared.WebAPI.ApiApto.Client   ( aptoRoutes )

getCardLastFour
  :: (MonadIO m, MonadThrow m) => Manager -> UserID -> m CardLastFour
getCardLastFour manager userId = do
  url <- parseBaseUrl aptoAPIUrl
  let env = mkClientEnv manager url
  traceId <- genTrace

  let fn     = _CardLastFour $ aptoRoutes env
  let spanId = 0
  let trace  = TraceContext { .. }

  liftIO $ fn trace userId
