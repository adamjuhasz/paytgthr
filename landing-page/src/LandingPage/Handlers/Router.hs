{-# LANGUAGE RecordWildCards #-}

module LandingPage.Handlers.Router where

import           Control.Concurrent.Async       ( concurrently )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( find )
import           Debug.Trace                    ( trace )
import           LandingPage.Types              ( UserAcceptedGroup(..)
                                                , UserAcceptedRatio(..)
                                                , UserSignupStep
                                                , nextPageChooser
                                                )
import           Shared.Amqp                    ( AMQPPublisher )
import           Shared.Amqp.Utils              ( getGroupForUser
                                                , getUser
                                                )
import           Shared.Models.Group            ( GroupMember
                                                  ( mbrAccepted
                                                  , mbrUser
                                                  )
                                                , GroupModel(..)
                                                , GroupSplit
                                                  ( splApproved
                                                  , splUser
                                                  )
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(usrUserID)
                                                )
import           Shared.Utils                   ( eitherToMaybe
                                                , fromRight
                                                )

isRatioAccepted :: UserID -> Maybe GroupModel -> UserAcceptedRatio
isRatioAccepted _ Nothing = RatioNotYet
isRatioAccepted user (Just GroupModel {..}) =
  case find ((== user) . splUser) grpSplit of
    Nothing -> trace errorMsg RatioNotYet
    Just x  -> if splApproved x then RatioAccepted else RatioNotYet
 where
  errorMsg =
    "Error: User not found in group split: "
      <> show user
      <> ", "
      <> show grpId
      <> ", "
      <> show grpRevision
      <> ", "
      <> show grpSplit

isGroupAccepted :: UserID -> Maybe GroupModel -> UserAcceptedGroup
isGroupAccepted _ Nothing = GroupNotYet
isGroupAccepted user (Just GroupModel {..}) =
  case find ((== user) . mbrUser) grpMembers of
    Nothing -> trace errorMsg GroupNotYet
    Just x  -> if mbrAccepted x then GroupAccepted else GroupNotYet
 where
  errorMsg =
    "Error: User not found in group members: "
      <> show user
      <> ", "
      <> show grpId
      <> ", "
      <> show grpRevision
      <> ", "
      <> show grpMembers

calculateNextStep :: UserModel -> Maybe GroupModel -> UserSignupStep
calculateNextStep userModel groupModel =
  let user              = usrUserID userModel
      userAcceptedGroup = isGroupAccepted user groupModel
      userAcceptedRatio = isRatioAccepted user groupModel
  in  nextPageChooser userModel userAcceptedGroup userAcceptedRatio

routerBase
  :: AMQPPublisher -> UserID -> IO (UserSignupStep, UserModel, Maybe GroupModel)
routerBase pub user = do
  let getTheUser  = getUser pub user <&> fromRight
      getTheGroup = getGroupForUser pub user <&> eitherToMaybe
  (userModel, groupModel) <- concurrently getTheUser getTheGroup

  let step = calculateNextStep userModel groupModel
  return (step, userModel, groupModel)
