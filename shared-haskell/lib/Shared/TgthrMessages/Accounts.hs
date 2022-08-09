{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use newtype instead of data" -}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StrictData, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Account-FSM messages
Description : Commands, Events and replies for account-fsm
Maintainer  : adam@example.com
Stability   : experimental
-}
module Shared.TgthrMessages.Accounts
  ( module Shared.TgthrMessages.Accounts
  , TokenMedium(..)
  , UserChanges(..)
  , FundingInformation(..)
  ) where

import           Data.Aeson                    as A
                                                ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value(String)
                                                , genericParseJSON
                                                , genericToJSON
                                                , withText
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Group            ( GroupId
                                                , GroupModel
                                                , GroupStatus
                                                )
import           Shared.Models.Payment          ( PaymentFailureCode(..) )
import           Shared.Models.Token            ( TokenId
                                                , TokenMedium(..)
                                                , TokenType
                                                )
import           Shared.Models.User             ( EmailAddress
                                                , FundingInformation(..)
                                                , Password
                                                , UserChanges(..)
                                                , UserID
                                                , UserModel
                                                , UserState
                                                )
import           Shared.TgthrMessages.Base      ( ThroughMQ(..) )
import           Shared.Utils                   ( customAesonOptions )

type WebResetURL = Text
data PasswordResetService = WebsiteToken WebResetURL
  deriving (Eq, Show, Generic)
instance FromJSON PasswordResetService where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON PasswordResetService where
  toJSON = genericToJSON customAesonOptions

data InviteMedium
  = EmailInvite EmailAddress
  | PhoneInvite Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AccountsCmd
  = GetUser -- GET /user/:uid
    { gucUser :: UserID
    }
  | GetUsersActivePendingGroups -- Get /groups/user/:uid?state=active&state=pending
    { gucUser :: UserID
    }
  deriving (Show, Eq, Generic)
instance FromJSON AccountsCmd where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AccountsCmd where
  toJSON = genericToJSON customAesonOptions
instance ThroughMQ AccountsCmd where
  toKey GetUser{}                     = "account.cmd.getuser"
  toKey GetUsersActivePendingGroups{} = "account.cmd.getusersgroups"

data UserLoginFailureReason
  = LoginSuccess
  | PasswordMismatch
  | UserDoesntExist
  | UserDetailsMissing
  | MQFailure
  deriving (Eq, Show)
instance FromJSON UserLoginFailureReason where
  parseJSON = withText "UserLoginFailureReason" $ \t -> case t of
    "passwordmismatch"   -> return PasswordMismatch
    "userdoesntexist"    -> return UserDoesntExist
    "loginsuccess"       -> return LoginSuccess
    "userdetailsmissing" -> return UserDetailsMissing
    "mqfailure"          -> return MQFailure
    _                    -> fail "Unknown UserLoginFailureReason"
instance ToJSON UserLoginFailureReason where
  toJSON PasswordMismatch   = String "passwordmismatch"
  toJSON UserDoesntExist    = String "userdoesntexist"
  toJSON LoginSuccess       = String "loginsuccess"
  toJSON UserDetailsMissing = String "userdetailsmissing"
  toJSON MQFailure          = String "mqfailure"

data AccountsEvent
  -- EventUserCreated
  = UserWasCreated
    { uceUser :: UserID
    }
  -- EventUserInfoChanged
  | UserWasUpdated
    { uueUser :: UserID
    , uueState :: UserState
    , uueChanges :: [UserChanges]
    }
  -- EventGroupCreated
  | GroupWasCreated
    { gceGroup :: GroupId
    , gceByUser :: UserID
    , gceAffectingUsers :: [UserID]
    }
  -- EventGroupInviteAccepted
  | UserAcceptedGroup
    { uleGroup :: GroupId
    , uleUser :: UserID
    , uleAffectingUsers :: [UserID]
    }
  -- EventGroupSplitChanged
  | GroupChangeSplit
    { cseGroup :: GroupId
    , cseByUser :: UserID
    , cseAffectingUsers :: [UserID]
    }
  -- EventGroupStateChanged GroupActive
  | GoupNowActive
    { saeGroup :: GroupId
    , saeAffectingUsers :: [UserID]
    }
  -- EventGroupStateChanged
  | GroupStateChange
    { gseGroupId :: GroupId
    , gseStatus :: GroupStatus
    , gseByUser :: UserID
    , gseAffectingUsers :: [UserID]
    }
  -- EventUserStateChanged
  | UserStateChange
    { useUser :: UserID
    , useStatus :: UserState
    }
  -- InviteInfo
  | UserWasInvited
    { uieInviterUser :: UserID
    , uieInviteeUser :: UserID
    , uieInviteeEmail :: EmailAddress
    , uieInviteeFName :: Text
    , uieGroupId :: GroupId
    }
  -- EventUserFSRemoved
  | FundingSourceRemoved
    { fseUser :: UserID
    , fseFundingInfo :: FundingInformation
    , fseACHReason :: Maybe PaymentFailureCode
    }
  -- TokenInfo
  | TokenCreated
    { cteUser :: UserID
    , cteTokenId :: TokenId
    , cteToken :: TokenType
    }
  deriving (Show, Eq, Generic)
instance FromJSON AccountsEvent where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AccountsEvent where
  toJSON = genericToJSON customAesonOptions
instance ThroughMQ AccountsEvent where
  toKey UserWasCreated{}       = "account.event.usercreated"
  toKey UserWasUpdated{}       = "account.event.userwasupdated"
  toKey GroupWasCreated{}      = "account.event.groupwascreated"
  toKey UserAcceptedGroup{}    = "account.event.useracceptedgroup"
  toKey GroupChangeSplit{}     = "account.event.groupchangesplit"
  toKey GoupNowActive{}        = "account.event.groupnowactive"
  toKey GroupStateChange{}     = "account.event.groupstatechange"
  toKey UserStateChange{}      = "account.event.userstatechange"
  toKey UserWasInvited{}       = "account.event.userwasinvited"
  toKey FundingSourceRemoved{} = "account.event.fundingsourceremoved"
  toKey TokenCreated{}         = "account.event.tokencreated"

data TokenVerificationFailure
  = TokenNotFound
  | TokenExpired
  | TokenAlreadyUsed
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AccountReplies
  = GetPasswordReply
    { gprEmail :: EmailAddress
    , gprUser :: UserID
    , gprPassword :: Password
    }
  | GetUserReply
    { gurUser :: UserID
    , gurUserModel :: UserModel
    }
  | GetGroupMembersReply
    { ggrUser :: UserID
    , ggrGroupId :: GroupId
    , ggrMembers :: [UserID]
    }
  | CreateUserReply
    { curUser :: UserID
    }
  | GetGroupReply
    { ghrGroupId :: GroupId
    , ghrGroupModel :: GroupModel
    }
  | GetUserGroupReply
    { gxrUser :: UserID
    , gxrGroups :: [GroupModel]
    }
  | GetAllActiveUsersReply
    { activeUsers :: [UserID]
    }
  | FoundUsersWithBankDetails
    { furUsers :: [UserID]
    }
  | SSNIsAvailable
    { snrIsAbailable :: Bool
    }
  | GroupListReply
    { glrGroups :: [GroupModel]
    }
  | TokenVerified
    { tvrUser :: UserID
    }
  | TokenNotVerified
    { tnrUser :: UserID
    , tnrReason :: TokenVerificationFailure
    }
  deriving (Eq, Show, Generic)
instance FromJSON AccountReplies where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AccountReplies where
  toJSON = genericToJSON customAesonOptions

data AccountReplyFailues
  = GetPasswordReplyFailure
    { gpfReason :: UserLoginFailureReason
    , gpfEmail :: EmailAddress
    }
  | GetUserReplyFailure
    { gufUser :: UserID
    }
  | InviteUserReplyFailure
    { iufReason :: UserLoginFailureReason
    }
  | GetGroupReplyFailure
    { ghfGroupId:: GroupId
    }
  deriving (Eq, Show, Generic)
instance FromJSON AccountReplyFailues where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AccountReplyFailues where
  toJSON = genericToJSON customAesonOptions

