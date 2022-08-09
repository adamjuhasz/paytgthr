{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- Needed for stack 17.10 for import           GHC.Generics                   ( Generic )
{-# LANGUAGE DeriveAnyClass #-}

module Shared.WebAPI.AccountsFSM.API
  ( module Shared.WebAPI.AccountsFSM.API
  , CreateCardholderAction(..)
  , UserTrait(..)
  , TraceContext(..)
  , CardActivateBody(..)
  , traceToMID
  , incrementTrace
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.HashMap.Strict           as Hm
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )
import           Servant
import           Servant.API.Generic            ( Generic
                                                , GenericMode(..)
                                                )
import           Shared.Models.Card             ( CardDesign
                                                , CardMemo
                                                , CardModel
                                                , CardPinEnc
                                                , CardStatus
                                                , IssuerPlatform
                                                )
import           Shared.Models.Cardholder       ( CardholderId )
import           Shared.Models.CategorySplit    ( CategoryCode
                                                , CategorySplit
                                                , CategoryState
                                                )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Group            ( GroupModel
                                                , GroupSplit
                                                , GroupStatus
                                                )
import           Shared.Models.Ids              ( ActivatedRewardId
                                                , CardId
                                                , GroupId(..)
                                                , ReferralProgramID
                                                , RewardId
                                                , TransactionId
                                                , UserID(..)
                                                )
import           Shared.Models.Invite           ( InviteCode
                                                , PartnerInvite
                                                )
import           Shared.Models.KYC              ( KycStatus )
import           Shared.Models.KYCAssesment     ( KYCAssesment )
import           Shared.Models.Payment          ( PaymentFailureCode
                                                , PaymentMethod
                                                )
import           Shared.Models.Referral.ReferralCode
import           Shared.Models.Referral.ReferralProgram
import           Shared.Models.Referral.ReferralProgress
import           Shared.Models.Rewards.Boost    ( RewardBoost )
import           Shared.Models.Token            ( ExpirationTime
                                                , TokenId
                                                , TokenMedium
                                                )
import           Shared.Models.User             ( ClosureReason
                                                , EmailAddress
                                                , Password
                                                , PhoneNumber
                                                , RedactedText
                                                , UserModel
                                                , UserState
                                                , UserTrait(..)
                                                )
import           Shared.TgthrMessages.Base      ( AccountType )
import           Shared.WebAPI.General.API      ( TraceContext(..)
                                                , TraceHeaders
                                                , incrementTrace
                                                , traceToMID
                                                )
import           Shared.WebAPI.General.Issuer   ( CardActivateBody(..)
                                                , CreateCardholderAction(..)
                                                )
import           System.Posix.ByteString        ( GroupID )

data CreateUserBody = CreateUserBody
  { email    :: EmailAddress
  , password :: Maybe Password
  }
  deriving (Eq, Show, Generic)
instance ToJSON CreateUserBody
instance FromJSON CreateUserBody

data RemoveBankFSBody = RemoveBankFSBody
  { removeSource :: PaymentMethod
  , achReason    :: Maybe PaymentFailureCode
  }
  deriving (Eq, Show, Generic)
instance ToJSON RemoveBankFSBody
instance FromJSON RemoveBankFSBody

data QueryBankFSBody = QueryBankFSBody
  { routingNumber :: RedactedText
  , accountNumber :: RedactedText
  }
  deriving (Eq, Show, Generic)
instance ToJSON QueryBankFSBody
instance FromJSON QueryBankFSBody

newtype CloseUserBody = CloseUserBody
  { reason :: ClosureReason
  }
  deriving (Eq, Show, Generic)
instance ToJSON CloseUserBody
instance FromJSON CloseUserBody

type FirstName = Text
data CreateGroupBody = CreateGroupBody
  { members      :: [(FirstName, EmailAddress, UserID)]
  , inviter      :: UserID
  , validThrough :: Maybe (UTCTime, UTCTime)
  }
  deriving (Eq, Show, Generic)
instance ToJSON CreateGroupBody
instance FromJSON CreateGroupBody

newtype ChangeKYCStateBody = ChangeKYCStateBody
  { newState :: KycStatus
  }
  deriving (Eq, Show, Generic)
instance ToJSON ChangeKYCStateBody
instance FromJSON ChangeKYCStateBody

data PasswordQueryResponse = PasswordQueryResponse
  { userId         :: UserID
  , hashedPassword :: Maybe Password
  }
  deriving (Eq, Show, Generic)
instance ToJSON PasswordQueryResponse
instance FromJSON PasswordQueryResponse

newtype CloseGroupBody = CloseGroupBody
  { closer :: UserID
  }
  deriving (Eq, Show, Generic)
instance ToJSON CloseGroupBody
instance FromJSON CloseGroupBody

data ChangeSplitBody = ChangeSplitBody
  { newSplits :: [GroupSplit]
  , changer   :: UserID
  }
  deriving (Eq, Show, Generic)
instance ToJSON ChangeSplitBody
instance FromJSON ChangeSplitBody

data ChangeCardStateBody = ChangeCardStateBody
  { cardState  :: CardStatus
  , cardDesign :: Maybe CardDesign
  , cardMemo   :: CardMemo
  }
  deriving (Eq, Show, Generic)
instance ToJSON ChangeCardStateBody
instance FromJSON ChangeCardStateBody

data SetCatSplit = SetCatSplit
  { category :: CategoryCode
  , split    :: [GroupSplit]
  , enabled  :: CategoryState
  }
  deriving (Eq, Show, Generic)
instance ToJSON SetCatSplit
instance FromJSON SetCatSplit

data UserCreateFSBody = UserCreateFSBody
  { bankName            :: Text
  , accountName         :: Text
  , achRouting          :: Text
  , achAccount          :: Text
  , verified            :: Bool
  , verificationAmounts :: [Currency]
  , accountType         :: AccountType
  }
  deriving (Eq, Show, Generic)
instance ToJSON UserCreateFSBody
instance FromJSON UserCreateFSBody

newtype IdentifyCellCarrier = IdentifyCellCarrier
  { cellCarrier :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON IdentifyCellCarrier
instance FromJSON IdentifyCellCarrier

data IdentifyDeviceIP = IdentifyDeviceIP
  { ipAddress :: Text
  , ipCountry :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON IdentifyDeviceIP
instance FromJSON IdentifyDeviceIP

newtype UpdateUserBody = UpdateUserBody { changes :: [(UserTrait, Maybe Text)] } deriving (Eq, Show, Generic)
instance ToJSON UpdateUserBody
instance FromJSON UpdateUserBody

newtype GetInviteResponse = GetInviteResponse
  { usersInvite :: Maybe PartnerInvite
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype AcceptInviteResponse = AcceptInviteResponse
  { newGroupId :: GroupId
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ChangePinBody = ChangePinBody
  { newPinText      :: Text
  , newPinEncrypted :: CardPinEnc
  }
  deriving (Eq, Show, Generic)
instance FromJSON ChangePinBody where
instance ToJSON ChangePinBody where

data ActivateRewardBody = ActivateRewardBody
  { activatedBy      :: UserID
  , rewardToActivate :: RewardId
  , newActivationId  :: ActivatedRewardId
  }
  deriving (Eq, Show, Generic)
instance FromJSON ActivateRewardBody where
instance ToJSON ActivateRewardBody where

data IdentifyDeviceBody = IdentifyDeviceBody
  { deviceId              :: Text
  , deviceName            :: Text
  , deviceBrand           :: Text
  , deviceManufacturer    :: Text
  , deviceModelName       :: Text
  , deviceModelId         :: Text
  , deviceProductName     :: Text
  , deviceOsVersion       :: Text
  , devicePlatform        :: Text
  , devicePlatformVersion :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON IdentifyDeviceBody where
instance ToJSON IdentifyDeviceBody where

newtype MarkRewardUsedBody = MarkRewardUsedBody
  { rewardUsedBy :: TransactionId
  }
  deriving (Eq, Show, Generic)
instance FromJSON MarkRewardUsedBody where
instance ToJSON MarkRewardUsedBody where

-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  -- /user/:uid
  { _UserGet                :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> Get '[JSON] UserModel
  , _UserCreate             :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> ReqBody '[JSON] CreateUserBody :> Post '[JSON] UserModel
  , _UserUpdate             :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> ReqBody '[JSON] UpdateUserBody :> Put '[JSON] NoContent
  , _UserAcceptDisclosure   :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "disclosure" :> Put '[JSON] NoContent
  , _UserAcceptConsent      :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "consent" :> Put '[JSON] NoContent
  , _UserSetPassword        :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "password" :> ReqBody '[JSON] Password :> Put '[JSON] NoContent
  , _UserCreateFS           :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "fundingsource" :> ReqBody '[JSON] UserCreateFSBody :> Post '[JSON] NoContent
  , _UserVerifyCurrentFS    :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "fundingsource" :> "verify" :> Post '[JSON] NoContent
  , _UserRemoveFS           :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "fundingsource" :> "bank" :> "remove" :> ReqBody '[JSON] RemoveBankFSBody :> Post '[JSON] NoContent
  , _UserChangeEmail        :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "email" :> ReqBody '[JSON] EmailAddress :> Put '[JSON] NoContent
  , _UserVerifyEmail        :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "email" :> "verify" :> Put '[JSON] NoContent
  , _UserClose              :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "close" :> ReqBody '[JSON] CloseUserBody :> Post '[JSON] NoContent
  , _UserRunKYC             :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "kyc" :> Post '[JSON] KYCAssesment
  , _UserSetKYCState        :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "kyc" :> ReqBody '[JSON] ChangeKYCStateBody :> Put '[JSON] NoContent
  , _UserSetCardState       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Capture "cardid" CardId :> ReqBody '[JSON] ChangeCardStateBody :> Put '[JSON] NoContent
  , _UserSetCardPin         :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Capture "cardid" CardId :> "pin" :> ReqBody '[JSON] ChangePinBody :> Put '[JSON] NoContent
  , _UserCreateCard         :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> "new" :> Capture "card_a" CardDesign :> Post '[JSON] NoContent
  , _UserCardList           :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Get '[JSON] [CardModel]
  , _UserCardActivate       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "card" :> Capture "cardid" CardId :> "activate" :> ReqBody '[JSON] CardActivateBody :> Post '[JSON] NoContent
  , _UserLockAllCards       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "cards" :> "lock" :> "admin" :> Post '[JSON] NoContent
  , _UserSendToPrivacy      :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "admin" :> "privacy" :> "send" :> Post '[JSON] CreateCardholderAction
  , _UserSendToDwolla       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "admin" :> "dwolla" :> "send" :> Post '[JSON] NoContent
  , _UserForceState         :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "admin" :> "state" :> Capture "newState" UserState :> Post '[JSON] NoContent
  , _UserVerifyPhone        :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "phone" :> "verify" :> Put '[JSON] NoContent
  , _UserIdentifyCarrier    :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "identify" :> "cellcarrier" :> ReqBody '[JSON] IdentifyCellCarrier :> Post '[JSON] NoContent
  , _UserIdentifyDeviceIP   :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "identify" :> "ip" :> ReqBody '[JSON] IdentifyDeviceIP :> Post '[JSON] NoContent
  , _UserIdentifyDevice     :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "identify" :> "device" :> ReqBody '[JSON] IdentifyDeviceBody :> Post '[JSON] NoContent
  , _UserMakeInvite         :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "invite" :> Post '[JSON] PartnerInvite
  , _UserGetInvite          :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "invite" :> Get '[JSON] PartnerInvite
  , _UserAcceptInvite       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "invite" :> "accept" :> Capture "inviteCode" InviteCode :> Post '[JSON]  AcceptInviteResponse

  -- /user/query
  , _UserGetPassword        :: route :- TraceHeaders :> "user" :> "query" :> "password" :> "email" :> ReqBody '[JSON] EmailAddress :> Get '[JSON] PasswordQueryResponse

  -- /users/
  , _UsersQueryAllActive    :: route :- TraceHeaders :> "users" :> "active" :> Get '[JSON] [UserID]
  , _UsersQueryCardholder   :: route :- TraceHeaders :> "users" :> "query" :> "cardholder" :> ReqBody '[JSON] CardholderId :> Post '[JSON] [UserModel]
  , _UsersQueryCard         :: route :- TraceHeaders :> "users" :> "query" :> "card" :> ReqBody '[JSON] IssuerPlatform :> Post '[JSON] [UserModel]
  , _UsersQueryPhone        :: route :- TraceHeaders :> "users" :> "query" :> "phone" :> ReqBody '[JSON] PhoneNumber :> Post '[JSON] [UserModel]
  , _UsersQueryEmail        :: route :- TraceHeaders :> "users" :> "query" :> "email" :> ReqBody '[JSON] EmailAddress :> Post '[JSON] [UserModel]
  , _UsersQueryBank         :: route :- TraceHeaders :> "users" :> "query" :> "bank" :> ReqBody '[JSON] QueryBankFSBody :> Post '[JSON] [UserModel]
  , _UsersQuerySSN          :: route :- TraceHeaders :> "users" :> "query" :> "ssn" :> ReqBody '[JSON] Text :> Post '[JSON] [UserModel]

  -- /group/:gid
  , _GroupGet               :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> Get '[JSON] GroupModel
  , _GroupCreate            :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> ReqBody '[JSON] CreateGroupBody :> Post '[JSON] GroupModel
  , _GroupClose             :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> ReqBody '[JSON] CloseGroupBody  :> Delete '[JSON] NoContent
  , _GroupApproveInvite     :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "membership" :> "approve" :> ReqBody '[JSON] UserID :> Put '[JSON] NoContent
  , _GroupDeclineInvite     :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "membership" :> "decline" :> ReqBody '[JSON] UserID :> Put '[JSON] NoContent
  , _GroupChangeSplit       :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "split" :> ReqBody '[JSON] ChangeSplitBody :> Put '[JSON] NoContent
  , _GroupApproveSplit      :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "split" :> "approve" :> ReqBody '[JSON] UserID :> Put '[JSON] NoContent
  , _GroupForceSplit        :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "split" :> "force" :> ReqBody '[JSON] [GroupSplit] :> Put '[JSON] NoContent
  , _GroupSetCategorySplits :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "split" :> "categories" :> ReqBody '[JSON] [SetCatSplit] :> Post '[JSON] NoContent
  , _GroupGetCategorySplits :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "split" :> "categories" :> Get '[JSON] [CategorySplit]
  , _GroupActivateReward    :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "rewards" :> "activate" :> ReqBody '[JSON] ActivateRewardBody :> Post '[JSON] [RewardBoost]
  , _GroupGetRewards        :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "rewards" :> Get '[JSON] [RewardBoost]
  , _GroupMarkRewardAsUsed  :: route :- TraceHeaders :> "group" :> Capture "groupid" GroupId :> "rewards" :> "mark" :> Capture "rewardId" RewardId :> ReqBody '[JSON] MarkRewardUsedBody :> Post '[JSON] NoContent

  -- /groups/
  , _GroupsForUser          :: route :- TraceHeaders :> "groups" :> "query" :> "user" :> Capture "userid" UserID :> QueryParams "state" GroupStatus :> Get '[JSON] [GroupModel]

  -- /rewards
  , _RewardsGetList         :: route :- TraceHeaders :> "rewards" :> Get '[JSON] [RewardBoost]
  , _RewardsGetSpecific     :: route :- TraceHeaders :> "rewards" :> Capture "RewardId" RewardId :> Get '[JSON] RewardBoost
  , _RewardsAddNew          :: route :- TraceHeaders :> "rewards" :> ReqBody '[JSON] RewardBoost :> Post '[JSON] NoContent
  , _RewardsGetAllEver      :: route :- TraceHeaders :> "rewards" :> "historical" :> Get '[JSON] [RewardBoost]
  , _RewardsEditSpecific    :: route :- TraceHeaders :> "rewards" :> Capture "RewardId" RewardId :> ReqBody '[JSON] RewardBoost :> Put '[JSON] NoContent
  , _RewardsCancelSpecific  :: route :- TraceHeaders :> "rewards" :> Capture "ActivatedRewardId" ActivatedRewardId :> Delete '[JSON] NoContent

  -- /referral
  , _GetReferralProgram     :: route :- TraceHeaders :> "referral" :> "program" :> Capture "ReferralProgramID" ReferralProgramID :> Get '[JSON] ReferralProgram
  , _SetReferralProgram     :: route :- TraceHeaders :> "referral" :> "program" :> ReqBody '[JSON] ReferralProgram  :> Post '[JSON] ReferralProgram
  , _GetRererralCode        :: route :- TraceHeaders :> "referral" :> "link" :> "code" :> Capture "referrer" UserID :> Get '[JSON] ReferralCode
  , _UseReferralCode        :: route :- TraceHeaders :> "referral" :> "link" :> "connect" :> Capture "Referral Code" ReferralCodeDisplay :> Capture "referrer" UserID :> Post '[JSON] NoContent
  , _GetReferralLinks       :: route :- TraceHeaders :> "referral" :> "link" :> "referees" :> Capture "referrer" UserID :> Get '[JSON] [ReferralProgress]
  , _GetReferralProgress    :: route :- TraceHeaders :> "referral" :> "progress" :> Capture "UserID" UserID :> Get '[JSON] (Maybe ReferralProgress)
  , _SetReferralProgress    :: route :- TraceHeaders :> "referral" :> "progress" :> ReqBody '[JSON] ReferralProgress :> Post '[JSON] NoContent
  , _UpdateReferralProgress :: route :- TraceHeaders :> "referral" :> "progress" :> Capture "UserID" UserID :> ReqBody '[JSON] WorkFlowProgress :> Post '[JSON] NoContent

  -- /token/:uid
  , _TokenCreate            :: route :- TraceHeaders :> Capture "userid" UserID :> "token" :> Capture "medium" TokenMedium :> Capture "expiration" ExpirationTime :> ReqBody '[JSON] TokenId :> Post '[JSON] NoContent
  , _TokenVerify            :: route :- TraceHeaders :> Capture "userid" UserID :> "token" :> Capture "medium" TokenMedium :> ReqBody '[JSON] Text :> Put '[JSON] Bool

  -- /card/
  , _GetCard                :: route :- TraceHeaders :> "card"  :> Capture "cardid" CardId :> Get '[JSON] (Maybe CardModel)
  , _FindCard               :: route :- TraceHeaders :> "cards" :> "query" :> "issuer" :> Capture "IssuerPlatform" IssuerPlatform :> Get '[JSON] (Maybe CardModel)

  -- admin
  , _AdminSyncSegment       :: route :- "admin" :> "sync" :> "segment" :> Post '[PlainText] NoContent
  , _AdminSyncUserSegment   :: route :- "admin" :> "sync" :> "segment" :> Capture "userid" UserID :> Post '[PlainText] NoContent
  , _AdminSetCardState      :: route :- TraceHeaders :> "admin" :> "user" :> Capture "userid" UserID :> "card" :> Capture "cardid" CardId :> ReqBody '[JSON] CardStatus :> Put '[JSON] NoContent
  , _AdminRecordUserNote    :: route :- TraceHeaders :> "admin" :> "user" :> Capture "userid" UserID :> "notes" :> ReqBody '[JSON] Text :> Put '[JSON] NoContent

  -- system
  , _health                 :: route :- "_health" :> Get '[PlainText] Text
  }
  deriving Generic

