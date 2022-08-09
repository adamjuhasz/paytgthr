{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}

module AFSM.Web.User.Change.Identify where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.Monad.HasEventTracking    ( HasEventTracking(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(getUserById) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(saveDeviceIP) )
import qualified AFSM.User.Close               as Close
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import qualified Data.Text                     as T
import           Servant                        ( ServerError
                                                , err404
                                                )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.User             ( ClosureReason(..)
                                                , UserID
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( IdentifyCellCarrier(..)
                                                , IdentifyDeviceBody(..)
                                                , IdentifyDeviceIP(..)
                                                , TraceContext
                                                )

isBannedCarrier :: T.Text -> Bool
isBannedCarrier carrier = any (`T.isInfixOf` normalizedCarrier) bannedCarriers
 where
  normalizer     = T.toLower . T.strip
  bannedCarriers = fmap
    normalizer
    [ "Beeline"
    , "CMHK"
    , "China Unicom"
    , "Glo LTE"
    , "Jazz"
    , "MPT"
    , "MTN"
    , "MTS.BY"
    , "MegaFon"
    , "Mobifone"
    , "Mobile Telesystems"
    , "Namaste"
    , "Orange"
    , "Plintron"
    , "Siminn"
    , "StarHub"
    , "TNT"
    , "Tele2"
    , "Telenor"
    , "Viettel"
    , "Warid"
    , "GiffGaff"
    , "Vodafone"
    , "Play"
    , "Golden Telecom"
    , "banglalink"
    , "MAROC Telecom"
    , "Taiwan Mobile"
    , "Etisalat"
    , "Glo NG"
    , "Zain Jo"
    , "Ooredoo"
    , "Mobile TeleSystems"
    , "TELCEL"
    , "DENT"
    ]
  normalizedCarrier = normalizer carrier

identifyCellCarrier
  :: (CanProcessUserEvents m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> IdentifyCellCarrier
  -> m NoContent
identifyCellCarrier trace uid IdentifyCellCarrier {..} = do
  userM <- getUserById uid
  user  <- case userM of
    Nothing -> throwError err404
    Just u  -> return u

  evts <- if isBannedCarrier cellCarrier
    then do
      trackEventWithProps uid "User Identified Carrier Banned"
        $ object ["carrier" .= cellCarrier]
      case usrUserState user of
        UserCreated -> Close.closeUser trace uid ForeignDeviceDuringSignup
        UserWaitingOnPII -> Close.closeUser trace uid ForeignDeviceDuringSignup
        UserWaitingOnKYC    -> return []
        UserKYCDelay        -> return []
        UserActive          -> return []
        UserUpdated         -> return []
        UserUpdatedKYCDelay -> return []
        UserClosed _        -> return []
    else return []

  mapM_ (processUserEvent trace) evts

  return NoContent

isBannedIpCountry :: T.Text -> Bool
isBannedIpCountry country = normalizedCountry `notElem` whitelistedCoutries
 where
  whitelistedCoutries = ["US"]
  normalizedCountry   = T.toUpper . T.strip $ country

identifyIPAddress
  :: (CanProcessUserEvents m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> IdentifyDeviceIP
  -> m NoContent
identifyIPAddress trace uid IdentifyDeviceIP {..} = do
  userM <- getUserById uid
  user  <- case userM of
    Nothing -> throwError err404
    Just u  -> return u

  saveDeviceIP trace uid (ipAddress, ipCountry)

  countryEvts <- if isBannedIpCountry ipCountry
    then do
      trackEventWithProps uid "User Identified IP OUS"
        $ object ["country" .= ipCountry, "ip" .= ipAddress]
      case usrUserState user of
        UserCreated -> Close.closeUser trace uid ForeignDeviceDuringSignup
        UserWaitingOnPII -> Close.closeUser trace uid ForeignDeviceDuringSignup
        UserWaitingOnKYC    -> return []
        UserKYCDelay        -> return []
        UserActive          -> return []
        UserUpdated         -> return []
        UserUpdatedKYCDelay -> return []
        UserClosed _        -> return []
    else return []

  let isIPBanned = case ipAddress of
        "24.186.140.133" -> True
        _                -> False

  bannedListEvts <- if isIPBanned
    then do
      trackEventWithProps uid "User Identified Banned IP"
        $ object ["country" .= ipCountry, "ip" .= ipAddress]
      Close.closeUser trace uid BannedIPAddress
    else return []

  mapM_ (processUserEvent trace) $ countryEvts <> bannedListEvts

  return NoContent

identifyDevice
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> IdentifyDeviceBody
  -> m NoContent
identifyDevice trace uid IdentifyDeviceBody {..} = do
  let isBanned = case deviceId of
        "oJoma9NcvyyYX7K4eUpYg" -> True
        "78919bb48a999b0d"      -> True
        "5135b26f0c8711b8"      -> True
        "c4d324b501a8b159"      -> True
        "00000000-0000-0000-0000-000000000000" -> True
        "00000000-0000-0000-0000-000000000000" -> True -- David Leija ring
        "a8f620b03a8699db"      -> True                -- David Leija ring
        "5dd51bbbe87e19ba"      -> True                -- David Leija ring
        "df075771fd13a1de"      -> True                -- David Leija ring
        "00000000-0000-0000-0000-000000000000" -> True -- David Leija ring
        "00000000-0000-0000-0000-000000000000" -> True -- David Leija ring
        "3aa372a9ed531a94"      -> True                -- David Leija ring
        "00000000-0000-0000-0000-000000000000" -> True -- David Leija ring
        "00000000-0000-0000-0000-000000000000" -> True -- David Leija ring
        "f1c81cf43a1e366a"      -> True -- Team thorpe ring
        "xquNtt0-qZHmjNcZf4BQ7" -> True -- Team thorpe ring
        "6MdR2SSkg3TzPBc_k5ZCN" -> True -- Team thorpe ring
        "0yOfr3p5bSlnC-TQX-DK-" -> True -- Team thorpe ring
        "e65cb01564c7ece0"      -> True                -- Team GoodWin.X
        "woJTd0n9ht0kq4Sy_66q6" -> True                -- Team GoodWin.X
        "00000000-0000-0000-0000-000000000000" -> True -- Team GoodWin.X
        "00000000-0000-0000-0000-000000000000" -> True -- Team GoodWin.X
        "00000000-0000-0000-0000-000000000000" -> True -- Team GoodWin.X
        "00000000-0000-0000-0000-000000000000" -> True -- Team GoodWin.X
        "e4a3dc12288ba991"      -> True -- Team Nagal.X
        "b0e85b9310245adc"      -> True -- Team Nagal.X
        "OI7ubhE9lcSp48bvOGhyA" -> True                -- Joshua Saunders
        "00000000-0000-0000-0000-000000000000" -> True -- Joshua Saunders
        "ee90f7996126ae2b"      -> True -- Laquita Wiggins gang
        "51c56d25a235e2dd"      -> True -- Michael Moreno
        "db4946dcd5a3655f"      -> True -- Iraqi group #1
        "T2pcvsYSyqZJ0iFtCj5Pq" -> True -- Iraqi group #1
        "qzKczp9Gw9pY-Cg5eZ3rJ" -> True -- Iraqi group #1
        "38152227eb59ba41"      -> True -- Shaun Nicolella
        _                       -> False

  bannedListEvts <- if isBanned
    then do
      trackEventWithProps uid "User Identified Banned Device"
        $ object ["deviceId" .= deviceId]
      Close.closeUser trace uid BannedDeviceId
    else return []

  mapM_ (processUserEvent trace) bannedListEvts

  return NoContent
