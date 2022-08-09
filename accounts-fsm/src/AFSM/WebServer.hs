{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AFSM.WebServer where

import           AFSM.AppMonad                  ( AppConfig
                                                , AppWebM(..)
                                                )
import           AFSM.Web.API                   ( genServerHandler )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Network.Wai                    ( Application )
import           Servant                        ( Handler )
import           Servant.Server.Generic         ( genericServeT )

-- no need to change below

appToHandler :: AppConfig -> AppWebM m -> Handler m
appToHandler cfg app = runReaderT (unAppWebM app) cfg

webserver :: AppConfig -> Application
webserver cfg = genericServeT (appToHandler cfg) genServerHandler
