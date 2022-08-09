{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Reduce duplication" -}

module AFSM.Web.User.Change.State
  ( changeUserState
  , closeUser
  , setKYCState
  , setCardState
  , adminSetCardState
  , adminlockAllCards
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import qualified AFSM.User.Change.Card         as Card
import           AFSM.User.Change.KYC           ( changeKYCState )
import qualified AFSM.User.Change.State        as State
import qualified AFSM.User.Close               as Close
import           Shared.Utils.Retry                     ( retryFn )
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Servant                        ( NoContent(..)
                                                , ServerError(..)
                                                , err403
                                                , err404
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Card             ( CardId
                                                , CardModel(..)
                                                , CardStatus(..)
                                                )
import           Shared.Models.Group            ( GroupMember(mbrUser)
                                                , GroupModel(grpMembers)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserState(..)
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( ChangeCardStateBody(..)
                                                , ChangeKYCStateBody(..)
                                                , CloseUserBody(..)
                                                , TraceContext
                                                )

changeUserState
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> UserState
  -> m NoContent
changeUserState trace userid state = do
  evts <- retryFn trace "State.changeState" $ State.changeState trace userid state
  mapM_ (processUserEvent trace) evts
  return NoContent

closeUser
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> CloseUserBody
  -> m NoContent
closeUser trace userid CloseUserBody {..} = do
  tracePrint trace "closeUser closing user: " (userid, reason)

  evts <- retryFn trace "Close.closeUser" $ Close.closeUser trace userid reason

  tracePrint trace "closeUser evts: " (userid, reason, evts)

  mapM_ (processUserEvent trace) evts
  return NoContent

setKYCState
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> ChangeKYCStateBody
  -> m NoContent
setKYCState trace userid ChangeKYCStateBody { newState = kycState } = do
  evts <- retryFn trace "changeKYCState" $ changeKYCState trace userid kycState
  mapM_ (processUserEvent trace) evts
  return NoContent

setCardState
  :: (CanProcessUserEvents m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> CardId
  -> ChangeCardStateBody
  -> m NoContent
setCardState trace userId cardId changes = do
  cardM <- getCard trace cardId
  CardModel { cardPlatform = issuerId } <- case cardM of
    Nothing -> do
      traceError trace
                 "Error: setCardState getCard could not find "
                 (userId, cardId)
      throwError err404
    Just c -> return c

  result <-
    retryFn trace "Card.changeCardState"
    $ Card.changeCardState userId trace issuerId
    $ Card.CardUpdate changes

  case result of
    Left (Card.IllegalStateChange e) -> do
      traceError trace
                 "Error: setCardState Card.changeCardState "
                 (userId, cardId, changes, e)
      throwError err403 { errBody = "Bad state change" }
    Right evts -> mapM_ (processUserEvent trace) evts

  return NoContent

adminSetCardState
  :: (CanProcessUserEvents m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> CardId
  -> CardStatus
  -> m NoContent
adminSetCardState trace userId cardId newStatus = do
  cardM <- getCard trace cardId
  CardModel { cardPlatform = issuerId } <- case cardM of
    Nothing -> do
      traceError trace
                 "Error: adminSetCardState getCard could not find "
                 (userId, cardId)
      throwError err404
    Just c -> return c

  result <-
    retryFn trace "Card.changeCardState"
    $ Card.changeCardState userId trace issuerId
    $ Card.CardAdminChange newStatus

  case result of
    Left (Card.IllegalStateChange e) -> do
      traceError trace
                 "Error: adminSetCardState Card.changeCardState "
                 (userId, cardId, newStatus, e)
      throwError err403
    Right evts -> mapM_ (processUserEvent trace) evts

  return NoContent

adminlockAllCards
  :: (CanProcessUserEvents m) => TraceContext -> UserID -> m NoContent
adminlockAllCards trace uid = do
  -- locks cards for any user linked to this user
  groups <- getGroupsForUser uid
  let usersAffiliated = fmap mbrUser . concatMap grpMembers $ groups

  mapM_ (lockCardForUser trace) usersAffiliated

  return NoContent

lockCardForUser :: (CanProcessUserEvents m) => TraceContext -> UserID -> m ()
lockCardForUser trace uid = do
  allCards <- getCardsFor trace uid
  let unlockedCards = filter
        (\CardModel {..} -> case cardStatus of
          CardCreated     -> True
          CardActive      -> True
          CardUserFrozen  -> True
          CardAdminFrozen -> False -- already locked
          CardClosed      -> False
        )
        allCards

  results <- mapM
    (\CardModel {..} -> Card.changeCardState uid trace cardPlatform
      $ Card.CardAdminChange CardAdminFrozen
    )
    unlockedCards

  tracePrint trace
             "adminlockAllCards Card.changeCardState "
             (uid, lefts results)

  mapM_ (processUserEvent trace) $ concat $ rights results
