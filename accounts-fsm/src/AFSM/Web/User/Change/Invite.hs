{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.User.Change.Invite where

import           AFSM.AppMonad                  ( CanProcessGroupEvents )
import           AFSM.Group.Create              ( createActiveGroup )
import           AFSM.IO.Random                 ( HasRandom(..) )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasEventTracking    ( (.=)
                                                , HasEventTracking(..)
                                                , object
                                                )
import           AFSM.Monad.HasInviteCode      as HIC
                                                ( HasInviteCode(..) )
import           AFSM.Web.Event.ProcessGroupEvents
                                                ( processGroupEvent )
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(..) )
import qualified Data.ByteString.Lazy.Char8    as C
import qualified Data.Text                     as T
import           Servant                        ( ServerError(errBody)
                                                , err404
                                                , err409
                                                )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( GroupId(GroupId)
                                                , UserID
                                                )
import           Shared.Models.Invite          as Inv
                                                ( InviteCode
                                                , InviteStatus(..)
                                                , PartnerInvite(..)
                                                , emptyPreFilled
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( AcceptInviteResponse(..)
                                                , TraceContext
                                                )
-- |
-- | Create a new invite
-- |
createInvite
  :: (HasInviteCode m, GetCurrentTime m, HasRandom m, HasEventTracking m)
  => TraceContext
  -> UserID
  -> m PartnerInvite
createInvite trace uid = do
  inviteExists <- getUsersInvite trace uid
  case inviteExists of
    Just i  -> return i
    Nothing -> do
      -- create random code
      randomCode <- getRandomInviteCode

      -- save invite
      now        <- getCurrentTime
      let invite = PartnerInvite { Inv.inviter      = uid
                                 , groupCreated     = Nothing
                                 , inviteCode       = randomCode
                                 , inviteePreFilled = emptyPreFilled
                                 , inviteStatus     = Created
                                 , inviteCreated    = now
                                 , inviteUpdated    = now
                                 , inviteRevision   = 1
                                 }
      saveInvite trace invite

      trackEventWithProps uid "User InviteCode Created"
        $ object ["code" .= randomCode]

      -- return to user
      return invite

-- |
-- | Accept an invite code, make an active group
-- |
acceptInvite
  :: (HasInviteCode m, CanProcessGroupEvents m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> InviteCode
  -> m AcceptInviteResponse
acceptInvite trace uid enteredCode = do
  let normalized = T.toUpper . T.replace "-" "" $ enteredCode

  inviteExists <- HIC.getInvite trace normalized
  invite       <- case inviteExists of
    Just i@PartnerInvite { inviteStatus = Created } -> return i
    -- check if invite already used up
    Just PartnerInvite { inviteStatus = Accepted }  -> throwError
      $ err409 { errBody = C.pack $ show normalized <> " already accepted" }
    Just PartnerInvite { inviteStatus = Cancelled } -> throwError
      $ err409 { errBody = C.pack $ show normalized <> " already cancelled" }
    -- check if invite exists
    Nothing -> throwError
      $ err404 { errBody = C.pack $ show normalized <> " code not found" }

  -- create group
  let theInviter = Inv.inviter invite
  when (theInviter == uid)
       (throwError err409 { errBody = "Can't accept your own invite" })

  newGID      <- GroupId <$> getUUID
  groupEvents <- createActiveGroup trace (theInviter, uid) newGID

  -- update invite as accepted
  now         <- getCurrentTime

  let modifiedInvite = invite { inviteStatus   = Accepted
                              , groupCreated   = Just newGID
                              , inviteUpdated  = now
                              , inviteRevision = inviteRevision invite + 1
                              }
  saveInvite trace modifiedInvite

  -- close other users invite
  myInvite <- getUsersInvite trace uid
  case myInvite of
    Nothing  -> return ()
    Just inv -> saveInvite trace $ inv
      { inviteStatus   = Cancelled
      , groupCreated   = Nothing
      , inviteUpdated  = now
      , inviteRevision = inviteRevision inv + 1
      }

  partnersInvite <- getUsersInvite trace theInviter
  case partnersInvite of
    Nothing  -> return ()
    Just inv -> saveInvite trace $ inv
      { inviteStatus   = Cancelled
      , groupCreated   = Nothing
      , inviteUpdated  = now
      , inviteRevision = inviteRevision inv + 1
      }

  trackEventWithProps uid "User InviteCode Accepted"
    $ object ["code" .= enteredCode]

  trackEventWithProps theInviter "User InviteCode Accepted"
    $ object ["code" .= enteredCode]

  tracePrint trace
             "acceptInvite "
             (uid, normalized, invite, newGID, groupEvents)
  mapM_ (processGroupEvent trace) groupEvents

  return $ AcceptInviteResponse newGID

-- |
-- | Get a user's invite to show the code in the GUI
-- |
getInvite
  :: (HasInviteCode m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> m PartnerInvite
getInvite trace uid = do
  -- get invite
  inviteExists <- getUsersInvite trace uid
  case inviteExists of
    Just i  -> return i
    Nothing -> throwError err404

