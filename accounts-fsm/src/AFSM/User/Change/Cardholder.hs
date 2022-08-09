module AFSM.User.Change.Cardholder where

import           AFSM.FSM.User                  ( UserEvent )
import           AFSM.IO.Time                   ( GetCurrentTime )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasIssuerClient     ( CreateCardholderAction(..)
                                                , HasIssuerClient
                                                  ( createCardholder
                                                  )
                                                )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB )
import           AFSM.User.Change.PII           ( changeUserPII )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Cardholder      as C
                                                ( CardholderId(..)
                                                , PrivacyAccountToken(..)
                                                )
import           Shared.Models.User            as U
                                                ( UserID
                                                , UserModel(..)
                                                , UserTrait(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

changeCardholderId
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> CardholderId
  -> m [UserEvent]
changeCardholderId trace userId cardholderId = do
  tracePrint trace "changeCardholderId " (userId, cardholderId)

  model  <- fromJust <$> getUserById userId

  events <- case cardholderId of
    AptoPaymentsCH _ -> error "Deprecated AptoPaymentsCH"
    PayWithPrivacyCH (C.PrivacyAccountToken cid) ->
      changeUserPII trace userId [(U.PrivacyAccountToken, Just cid)]

  newModel <- fromJust <$> getUserById userId

  -- print debug diff
  diffUser trace model newModel

  tracePrint trace "changeCardholderId done " (userId, cardholderId, events)

  return events

getCardHolderId
  :: ( HasIssuerClient m
     , HasGetUserDB m
     , HasSaveUserDB m
     , GetCurrentTime m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> m (CreateCardholderAction, [UserEvent])
getCardHolderId trace userId = do
  user          <- getUserById userId

  cardholderRes <- case user of
    Nothing ->
      error $ "Error: getCardHolderId getUserById is Nothing " <> show userId
    Just UserModel { usrPrivacyAcctToken = Just _token } ->
      return CardholderNotUpdated
    Just UserModel{} -> createCardholder trace userId

  let cardholderIdMaybe = case cardholderRes of
        CardholderCreated i  -> Just i
        CardholderUpdated i  -> Just i
        CardholderNotUpdated -> Nothing

  evts <- case cardholderIdMaybe of
    Nothing           -> return []
    Just cardholderId -> changeCardholderId trace userId cardholderId

  return (cardholderRes, evts)
