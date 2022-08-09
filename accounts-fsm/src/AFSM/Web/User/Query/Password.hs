{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AFSM.Web.User.Query.Password
  ( getPasswordForUser
  ) where

import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.User.Query.Password       ( UserPassword(..)
                                                , getPasswordForEmail
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Servant                        ( ServerError
                                                , err404
                                                )
import           Shared.Models.User             ( EmailAddress )
import           Shared.WebAPI.AccountsFSM.API  ( PasswordQueryResponse(..)
                                                , TraceContext
                                                )

getPasswordForUser
  :: (MonadError ServerError m, HasGetUserDB m)
  => TraceContext
  -> EmailAddress
  -> m PasswordQueryResponse
getPasswordForUser _ email = do
  res <- getPasswordForEmail email
  case res of
    UserNotExist      -> throwError err404
    PasswordEmpty uid -> return $ PasswordQueryResponse uid Nothing
    PasswordIs uid password ->
      return $ PasswordQueryResponse uid $ Just password
