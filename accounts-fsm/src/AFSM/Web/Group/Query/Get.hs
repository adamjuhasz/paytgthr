{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AFSM.Web.Group.Query.Get
  ( getGroupByID
  ) where

import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Servant                        ( ServerError
                                                , err404
                                                )
import           Shared.Models.Group            ( GroupId
                                                , GroupModel
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( TraceContext )

getGroupByID
  :: (MonadError ServerError m, HasGetGroupDB m)
  => TraceContext
  -> GroupId
  -> m GroupModel
getGroupByID _ groupid = do
  res <- getGroupByGroupId groupid
  case res of
    Just g  -> return g
    Nothing -> throwError err404
