{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AFSM.Web.User.Change.Create
  ( createUser
  , isBannedDomain
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(saveUserNote) )
import           AFSM.User.Close                ( closeUser )
import qualified AFSM.User.Create              as AFSM
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           Shared.Console                 ( tracePrint )
import           Shared.Models.User             ( ClosureReason(..)
                                                , EmailAddress(..)
                                                , UserID
                                                , UserModel
                                                )
import           Shared.Track.HasTracing        ( HasTracing(..) )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.AccountsFSM.API  ( CreateUserBody(..)
                                                , TraceContext
                                                , incrementTrace
                                                , traceToMID
                                                )

isBannedDomain :: EmailAddress -> Bool
isBannedDomain (EmailAddress rawEmail) = any (`T.isInfixOf` normalizedEmail)
                                             bannedDomains
 where
  normalizer      = T.toLower . T.strip
  bannedDomains   = fmap normalizer ["yopmail.com"]
  normalizedEmail = normalizer rawEmail

createUser
  :: (CanProcessUserEvents m, HasTracing m)
  => TraceContext
  -> UserID
  -> CreateUserBody
  -> m UserModel
createUser trace userid CreateUserBody {..} = do
  let messageId = traceToMID trace
  newTrace <- incrementTrace trace

  let emailIsBanned = isBannedDomain email

  let noFirstName   = Nothing
  (user, evts) <-
    traceSpan "AFSM.User.Create.createUser" trace newTrace
    $ retryFn trace "AFSM.createUser"
    $ AFSM.createUser messageId email password noFirstName userid

  banEvts <- if emailIsBanned
    then do
      tracePrint trace "Tried to use a banned domain " (userid, email)
      closeEvt <- closeUser trace user FraudyUser
      saveUserNote trace user ("Auto locked for banned domain", 1)
      return closeEvt
    else return []

  mapM_ (processUserEvent trace) $ evts <> banEvts

  fromJust <$> getUserById user
