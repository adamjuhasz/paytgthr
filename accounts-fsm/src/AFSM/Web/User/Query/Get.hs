{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AFSM.Web.User.Query.Get
  ( getUserByID
  , getCardsForUser
  ) where

import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Servant                        ( ServerError
                                                , err404
                                                )
import           Shared.Models.Card             ( CardModel )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( TraceContext )

getUserByID
  :: (MonadError ServerError m, HasGetUserDB m)
  => TraceContext
  -> UserID
  -> m UserModel
getUserByID _ userid = do
  res <- getUserById userid
  case res of
    Just u  -> return u
    Nothing -> throwError err404

getCardsForUser :: (HasGetUserDB m) => TraceContext -> UserID -> m [CardModel]
getCardsForUser = getCardsFor
