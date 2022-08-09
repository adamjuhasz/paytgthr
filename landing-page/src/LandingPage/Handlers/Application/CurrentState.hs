{-# LANGUAGE RecordWildCards #-}
module LandingPage.Handlers.Application.CurrentState where

import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(Null)
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isJust
                                                , listToMaybe
                                                )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Handlers.Router    ( isGroupAccepted
                                                , isRatioAccepted
                                                )
import           LandingPage.Types              ( SessionData
                                                , UserAcceptedGroup(..)
                                                , UserAcceptedRatio(..)
                                                )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types.Status      ( status500 )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Group            ( GroupMember(..)
                                                , GroupModel(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.Transactions.CheckTransactionAbility
                                                ( CanDoTrx(..)
                                                , canUserMakeTrx
                                                )
import           Shared.Transactions.UserGroupConsistencyCheck
                                                ( GroupCheckFailure(..)
                                                , UserCheckFailure(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , asClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                , setHeader
                                                , status
                                                )

getCurrentState :: V.Key SessionData -> Maybe UserID -> ClientEnv -> ActionM ()
getCurrentState sKey userMaybe accountsEnv = do
  user <- case userMaybe of
    Just u -> return u
    Nothing ->
      request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  let getUserFn = _UserGet asClientM trace user
  userModelE <- liftAndCatchIO $ runClientM getUserFn accountsEnv
  userModel  <- case userModelE of
    Left e -> do
      traceError trace "Error: can't get user" (user, e)
      status status500 >> json (object []) >> finish
    Right u -> return u

  let getGroupFn = _GroupsForUser asClientM trace user [GroupActive]
  groupModelE <- liftAndCatchIO $ runClientM getGroupFn accountsEnv
  groups      <- case groupModelE of
    Left e -> do
      traceError trace "Error: can't get group" (user, e)
      status status500 >> json (object []) >> finish
    Right g -> return g
  let groupM = listToMaybe groups

  isTrxAGo <- liftAndCatchIO $ canUserMakeTrx accountsEnv trace user groups

  partner :: Maybe Value <- case groupM of
    Nothing    -> return Nothing
    Just group -> do
      let groupMembers = mbrUser <$> grpMembers group
      let partner      = filter (/= user) groupMembers
      case partner of
        []             -> return Nothing -- no partner ?!
        _ : _ :      _ -> return Nothing -- multiple partners, no idea what to do
        [     pUserID] -> do
          let getPartner = _UserGet asClientM trace pUserID
          partnerM <- liftAndCatchIO $ runClientM getPartner accountsEnv
          case partnerM of
            Left  _              -> return Nothing
            Right UserModel {..} -> return . Just $ object
              [ "linked" .= isJust usrBankVerified
              , "verified" .= (usrBankVerified == Just True)
              , "status" .= usrUserState
              , "firstName" .= usrFirstName
              , "lastName" .= usrLastName
              ]

  setHeader "cache-control" "no-cache"
  let
    outObject = object
      [ "user" .= object
        [ "id" .= usrUserID userModel
        , "revision" .= usrRevision userModel
        , "email" .= usrEmail userModel
        , "emailVerified" .= usrEmailVerified userModel
        , "status" .= usrUserState userModel
        , "name" .= object
          ["first" .= usrFirstName userModel, "last" .= usrLastName userModel]
        , "address" .= object
          [ "street" .= usrAddressStreet userModel
          , "street2" .= usrAddressStreet2 userModel
          , "city" .= usrAddressCity userModel
          , "state" .= usrAddressState userModel
          , "zip" .= usrAddressZip userModel
          ]
        , "ach" .= object
          [ "abaExists" .= isJust (usrBankRouting userModel)
          , "ddaExists" .= isJust (usrBankAcount userModel)
          , "bankName" .= usrBankName userModel
          , "accountName" .= usrBankAccountName userModel
          , "verified" .= usrBankVerified userModel
          , "dwollaExists" .= isJust (usrDwollaId userModel)
          , "dwollaFSExists" .= isJust (usrDwollaFundingId userModel)
          ]
        , "phone" .= object
          [ "number" .= usrPhone userModel
          , "verified" .= usrPhoneVerified userModel
          ]
        , "dob" .= usrDOB userModel
        , "ssn" .= object ["exists" .= isJust (usrSSN userModel)]
        , "disclosure" .= usrDislcosureOk userModel
        , "consent" .= usrConstentOk userModel
        , "kyc" .= object ["status" .= usrAptoKYCStatus userModel]
        , "card" .= object
          [ "status" .= usrAptoCardStatus userModel
          , "enabled" .= case isTrxAGo of
            EverythingOK -> True
            _            -> False
          , "disabledReason" .= case isTrxAGo of
            EverythingOK                             -> Null
            ErrorAccessingUser                       -> "ErrorAccessingUser"
            GroupCantDoTrx   NoActiveGroup           -> "NoActiveGroup"
            GroupCantDoTrx   MultiplePermGroups      -> "MultiplePermGroups"
            UserCantDoTrx    UserIsNotActive         -> "UserIsNotActive"
            UserCantDoTrx    UserIsClosed            -> "UserIsClosed"
            UserCantDoTrx    NoFundingSourceLinked   -> "NoFSLinked"
            UserCantDoTrx    NoFundingSourceVerified -> "NoFSVerified"
            UserCantDoTrx    KYCNotPassed            -> "KYCNotPassed"
            PartnerCantDoTrx UserIsNotActive         -> "PartnerIsNotActive"
            PartnerCantDoTrx UserIsClosed            -> "PartnerIsClosed"
            PartnerCantDoTrx NoFundingSourceLinked   -> "PartnerNoFSLinked"
            PartnerCantDoTrx NoFundingSourceVerified -> "PartnerNoFSVerified"
            PartnerCantDoTrx KYCNotPassed            -> "PartnerKYCNotPassed"
          ]
        , "timestamps" .= object
          [ "created" .= usrCreatedOn userModel
          , "activated" .= usrBecameActiveOn userModel
          ]
        ]
      , "group" .= object
        [ "id" .= (grpId <$> groupM)
        , "status" .= (grpStatus <$> groupM)
        , "revision" .= (grpRevision <$> groupM)
        , "ratioAccepted" .= (isRatioAccepted user groupM == RatioAccepted)
        , "groupAccepted" .= (isGroupAccepted user groupM == GroupAccepted)
        ]
      , "partner" .= partner
      , "version" .= (1 :: Int)
      ]

  tracePrint trace "getCurrentState outObject " (user, outObject)
  Scotty.json outObject
