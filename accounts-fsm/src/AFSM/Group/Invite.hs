{-# LANGUAGE FlexibleContexts #-}

module AFSM.Group.Invite where

import           AFSM.FSM.Group                 ( GroupEvent )
import           AFSM.FSM.User                  ( UserEvent )
import           AFSM.Group.Create              ( InviteInfo(..)
                                                , InvtingError(..)
                                                , createNewGroup
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB )
import           Control.Exception              ( throw )
import           Control.Monad                  ( when )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.UUID.V4                   ( nextRandom )
import           Shared.Models.Ids              ( GroupId )
import           Shared.Models.User             ( EmailAddress
                                                , UserID(..)
                                                , UserModel(..)
                                                , normalizeEmail
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

type InviterUserID = UserID
type InviteeFirstName = Text
type InviteeEmail = EmailAddress
inviteUser
      :: ( HasGetUserDB m
         , HasGetGroupDB m
         , HasSaveGroupDB m
         , MonadIO m
         , GetCurrentTime m
         , HasSaveUserDB m
         )
      => TraceContext
      -> GroupId
      -> InviterUserID
      -> InviteeFirstName
      -> InviteeEmail
      -> m ([UserEvent], [GroupEvent], [InviteInfo], GroupId)
inviteUser trace groupId inviterUID inviteeFName inviteeEmail = do
      modelMaybe <- getUserById inviterUID
      let model        = fromMaybe (throw InviterMissing) modelMaybe
      let inviterEmail = usrEmail model

      when (normalizeEmail inviterEmail == normalizeEmail inviteeEmail)
           (throw CantInviteYourself)

      let inviterFname = case model of
                UserModel { usrFirstName = Nothing } ->
                      throw InviterMissingFirstName
                UserModel { usrFirstName = Just f } -> f

      inviteeUserID <- UserID <$> liftIO nextRandom

      let members =
                [ (inviterFname, usrEmail model             , usrUserID model)
                , (inviteeFName, normalizeEmail inviteeEmail, inviteeUserID)
                ]

      createNewGroup trace groupId inviterUID members
