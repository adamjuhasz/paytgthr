{- HLINT ignore "Use let" -}

module AFSM.User.Tools.Diff where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.User             ( UserModel(..) )
import           Shared.WebAPI.General.API      ( TraceContext )

diffUser :: (MonadIO m) => TraceContext -> UserModel -> UserModel -> m ()
diffUser trace user1 user2 = do
  let compareUsers f = f user1 == f user2
  let showDiff f name =
        name
          <> (":: User1: " <> show (f user1))
          <> (", User2: " <> show (f user2))
          <> " ## "
  let compreElement f name = if compareUsers f then "" else showDiff f name

  diff <-
    return
    $  (if compareUsers usrUserID
         then "Diff of " <> show (usrUserID user1) <> " ## "
         else ""
       )
    <> compreElement usrUserID             "usrUserID"
    <> compreElement usrUserState          "usrUserState"
    <> compreElement usrVersion            "usrVersion"
    <> compreElement usrEmail              "usrEmail"
    <> compreElement usrPassword           "usrPassword"
    <> compreElement usrFirstName          "usrFirstName"
    <> compreElement usrLastName           "usrLastName"
    <> compreElement usrAddressStreet      "usrAddressStreet"
    <> compreElement usrAddressStreet2     "usrAddressStreet2"
    <> compreElement usrAddressCity        "usrAddressCity"
    <> compreElement usrAddressState       "usrAddressState"
    <> compreElement usrAddressZip         "usrAddressZip"
    <> compreElement usrBankRouting        "usrBankRouting"
    <> compreElement usrBankAcount         "usrBankAcount"
    <> compreElement usrBankAccountName    "usrBankAccountName"
    <> compreElement usrBankName           "usrBankName"
    <> compreElement usrBankType           "usrBankType"
    <> compreElement usrBankVerified       "usrBankVerified"
    <> compreElement usrBankVerifedAmounts "usrBankVerifedAmounts"
    <> compreElement usrDwollaId           "usrDwollaId"
    <> compreElement usrDwollaFundingId    "usrDwollaFundingId"
    <> compreElement usrPhone              "usrPhone"
    <> compreElement usrDOB                "usrDOB"
    <> compreElement usrSSN                "usrSSN"
    <> compreElement usrDislcosureOk       "usrDislcosureOk"
    <> compreElement usrConstentOk         "usrConstentOk"
    <> compreElement usrAptoCardholderID   "usrAptoCardholderID"
    <> compreElement usrAptoKYCStatus      "usrAptoKYCStatus"
    <> compreElement usrAptoCardId         "usrAptoCardId"
    <> compreElement usrAptoCardStatus     "usrAptoCardStatus"
    <> compreElement usrCreatedOn          "usrCreatedOn"
    <> compreElement usrFirstSignIn        "usrFirstSignIn"
    <> compreElement usrBecameActiveOn     "usrBecameActiveOn"
    <> compreElement usrCardCreatedOn      "usrCardCreatedOn"
    <> compreElement usrCardActivatedOn    "usrCardActivatedOn"
    <> compreElement usrRevision           "usrRevision"
    <> compreElement usrMsgSource          "usrMsgSource"

  tracePrint trace "diffUser" diff

