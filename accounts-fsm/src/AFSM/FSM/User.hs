{-# LANGUAGE StrictData, RecordWildCards #-}

{-|
Module      : FSM
Description : Finite state machine functions for user and group
Maintainer  : adam@example.com
Stability   : experimental
-}
module AFSM.FSM.User
  ( createAccount
  , updateUserFunction
  , setUserState
  , sendStateChangeEvents
  , updateKYCState
  , updateAptoCardState
  , updatePassword
  , removeFunding
  , addNewFunding
  , addManualFunding
  , verifyManualFunding
  , increaseUserRevision
  , closeAccount
  , acceptDisclosure
  , acceptConsent
  , setUserDates
  , updateAccount
  , unShadowAccount
  , changeEmail
  , adminSetState
  , verifyEmailAddresss
  , verifyPhoneNumber
  , AccountName(..)
  , ABARouting(..)
  , DDANumber(..)
  , BankName(..)
  , AccountType
  , UserEvent(..)
  , ACHFundingSource(..)
  ) where

import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , isNothing
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Stack                      ( HasCallStack )
import           Shared.Models.Card             ( AptoCardId(..)
                                                , CardModel
                                                , CardStatus(..)
                                                )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                )
import           Shared.Models.KYC              ( KycStatus(..) )
import           Shared.Models.Payment          ( PaymentFailureCode )
import           Shared.Models.User            as U
                                                ( ClosureReason(..)
                                                , EmailAddress(..)
                                                , FundingInformation(..)
                                                , Password(..)
                                                , PhoneNumber(..)
                                                , RedactedText(RedactedText)
                                                , UserChanges(..)
                                                , UserID(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                , UserTrait(..)
                                                , defaultUser
                                                , normalizeEmail
                                                )
import           Shared.TgthrMessages.Base      ( AccountType(Depository)
                                                , DepositoryType(..)
                                                , MessageID
                                                )
import           Shared.Utils                   ( stringToDate )

type PreviousCardStatus = CardStatus

data ACHFundingSource = ACHFundingSource
  { achBankName            :: Text
  , achAccountName         :: Text
  , achABARouting          :: Text
  , achDDANumber           :: Text
  , achVerified            :: Bool
  , achVerificationAmounts :: [Currency]
  }
  deriving (Eq, Show)

data UserEvent
    = EventUserCreated UserID -- UserWasCreated { uceUser = userid }
    | EventUserStateChanged UserID UserState -- 
    | EventUserInfoChanged UserID UserState [UserChanges] -- UserWasUpdated
    | EventUserFSAdded UserID ACHFundingSource
    | EventUserFSVerified UserID FundingInformation
    | EventUserFSRemoved UserID FundingInformation (Maybe PaymentFailureCode)
    | EventUserPasswordChanged UserID
    | EventUserEmailChanged UserID
    | EventUserCardStateChangedFromTo PreviousCardStatus CardModel
    | EventUserKYCStateChangedFromTo UserID (Maybe KycStatus) KycStatus
    | EventUserEmailVerified UserID
    | EventUserPhoneVerified UserID
    | EventUserCardCreated UserID CardModel
  deriving (Eq, Show)

type FSMUserResult = ([UserEvent], UserModel)

createAccount
  :: UTCTime
  -> MessageID
  -> UserID
  -> EmailAddress
  -> Maybe Password
  -> ([UserEvent], UserModel)
createAccount now mid userid email password =
  let newUser = (defaultUser now $ U.normalizeEmail email)
        { usrUserID    = userid
        , usrPassword  = password
        , usrMsgSource = mid
        }
      createdEvent = EventUserCreated userid
  in  ([createdEvent], newUser)

updatedStatus :: UserModel -> UserState
updatedStatus UserModel {..} = case usrUserState of
  UserActive -> U.UserUpdated
  _          -> UserWaitingOnPII

updateBankType :: Maybe Text -> Maybe AccountType
updateBankType (Just "checking") = Just $ Depository Checking
updateBankType (Just "savings" ) = Just $ Depository Savings
updateBankType (Just _         ) = Nothing
updateBankType Nothing           = Nothing

-- inline brittany config for width
-- brittany-next-binding --columns 500
updateUserFunction :: UserTrait -> Maybe Text -> UserModel -> UserModel
updateUserFunction key value model = case key of
  NameFirst            -> model { usrFirstName = value }
  NameLast             -> model { usrLastName = value }
  AddressStreet        -> model { usrAddressStreet = value, usrUserState = updatedStatus model }
  AddressStreet2       -> model { usrAddressStreet2 = value, usrUserState = updatedStatus model }
  AddressCity          -> model { usrAddressCity = value, usrUserState = updatedStatus model }
  AddressState         -> model { usrAddressState = value, usrUserState = updatedStatus model }
  AddressZip           -> model { usrAddressZip = value, usrUserState = updatedStatus model }
  FsACHBankRoutingNum  -> model { usrBankRouting = RedactedText <$> value }
  FsACHBankAccountNum  -> model { usrBankAcount = RedactedText <$> value }
  DateOfBirth          -> model { usrDOB = stringToDate . unpack <$> value, usrUserState = updatedStatus model }
  EncryptedSSN         -> model { usrSSN = RedactedText <$> value, usrUserState = updatedStatus model }
  Phone                -> model { usrPhone = PhoneNumber <$> value, usrPhoneVerified = False }
  PrivacyAccountToken  -> model { usrPrivacyAcctToken = value }
  FsACHBankName        -> model { usrBankName = value }
  FsACHBankAccountName -> model { usrBankAccountName = value }
  FsACHBankAccountType -> model { usrBankType = updateBankType value }
  DwollaCustomerId     -> model { usrDwollaId = value }
  DwollaFundingId      -> model { usrDwollaFundingId = value }

data UserEntry
  = EntryIncomplete
  | CompletedEntry
  deriving (Eq, Show)

isComplete :: UserModel -> UserEntry
isComplete UserModel {..} =
  let nameFilledOut    = isJust usrFirstName && isJust usrLastName
      addressFilledOut = and
        [ isJust usrAddressStreet -- Street2 is not required
        , isJust usrAddressCity
        , isJust usrAddressState
        , isJust usrAddressZip
        ]
      legalAccepted = isJust usrDislcosureOk && isJust usrConstentOk
      allFilled     = and
        [ isJust usrPassword
        , nameFilledOut
        , addressFilledOut
        , isJust usrPhone
        , isJust usrDOB
        , isJust usrSSN
        , legalAccepted
        ]
  in  if allFilled then CompletedEntry else EntryIncomplete

-- inline brittany config for width
-- brittany-next-binding --columns 500
setUserState :: ([UserEvent], UserModel) -> ([UserEvent], UserModel)
setUserState (evts, model) =
  let newState = case (usrUserState model, isComplete model, usrAptoKYCStatus model) of
        (UserCreated        , CompletedEntry , _          ) -> UserWaitingOnKYC
        (UserCreated        , EntryIncomplete, _          ) -> UserWaitingOnPII
        (UserWaitingOnPII   , EntryIncomplete, _          ) -> UserWaitingOnPII
        (UserWaitingOnPII   , CompletedEntry , _          ) -> UserWaitingOnKYC
        (UserWaitingOnKYC   , _              , Just Passed) -> UserActive
        (UserWaitingOnKYC   , _              , _          ) -> UserWaitingOnKYC
        (UserKYCDelay       , _              , Just Passed) -> UserActive
        (UserKYCDelay       , _              , _          ) -> UserKYCDelay
        (UserUpdated        , EntryIncomplete, _          ) -> UserUpdated
        (UserUpdated        , CompletedEntry , _          ) -> UserUpdatedKYCDelay
        (UserUpdatedKYCDelay, _              , Just Passed) -> UserActive
        (UserUpdatedKYCDelay, _              , _          ) -> UserUpdatedKYCDelay
        (UserClosed r       , _              , _          ) -> UserClosed r
        (UserActive         , EntryIncomplete, _          ) -> UserUpdated
        (UserActive         , _              , _          ) -> UserActive
  in  (evts, model { usrUserState = newState })

adminSetState :: UserState -> FSMUserResult -> FSMUserResult
adminSetState newState (evts, orig) = (evts, orig { usrUserState = newState })

-- inline brittany config for width
-- brittany-next-binding --columns 200
sendStateChangeEvents :: UserModel -> ([UserEvent], UserModel) -> ([UserEvent], UserModel)
sendStateChangeEvents orig (evts, current) = (allEvts, current)
 where
  userId         = usrUserID current
  allEvts        = evts <> [ stateEvent | usrUserState orig /= usrUserState current ] <> [ updatedEvent | not (null changes) ]
  stateEvent     = EventUserStateChanged userId $ usrUserState current
  updatedEvent   = EventUserInfoChanged userId (usrUserState current) changes
  nameChanged    = diff [usrFirstName, usrLastName] orig current
  addressChanged = diff [usrAddressStreet, usrAddressStreet2, usrAddressCity, usrAddressZip, usrAddressState] orig current
  bankChanged    = diff [usrBankAcount, usrBankRouting] orig current
  phoneChanged   = usrPhone orig /= usrPhone current
  piiChanged     = (usrDOB orig /= usrDOB current) || (usrSSN orig /= usrSSN current)
  cardChanged    = usrAptoCardId orig /= usrAptoCardId current
  kycChanged     = usrAptoKYCStatus orig /= usrAptoKYCStatus current
  emailChanged   = usrEmail orig /= usrEmail current
  changes =
    [ UsersName | nameChanged ]
      <> [ UsersAddress | addressChanged ]
      <> [ UsersBank | bankChanged ]
      <> [ UsersPhone | phoneChanged ]
      <> [ UsersPII | piiChanged ]
      <> [ UsersCard | cardChanged ]
      <> [ UsersKYC | kycChanged ]
      <> [ UsersEmail | emailChanged ]
  diff fs o n = or $ fmap (\f -> f o /= f n) fs

updateAccount
  :: MessageID -> [(UserTrait, Maybe Text)] -> FSMUserResult -> FSMUserResult
updateAccount mid changes (evts, orig) = (evts, updatedModel)
 where
  updatedModel =
    (foldrWithKey updateUserFunction orig changes) { usrMsgSource = mid }
  foldrWithKey fn = foldr (\(key, val) accum -> fn key val accum)
unShadowAccount
  :: UTCTime -> MessageID -> Maybe Password -> UserModel -> ([a], UserModel)
unShadowAccount now mid cucPassword orig =
  ( []
  , orig
    { usrPassword    = cucPassword
    , usrMsgSource   = mid
    , usrFirstSignIn = if isNothing (usrFirstSignIn orig)
                         then Just now
                         else usrFirstSignIn orig
    }
  )

updateKYCState
  :: HasCallStack => MessageID -> KycStatus -> UserModel -> FSMUserResult
updateKYCState mid kycStatus orig =
  ( [ EventUserKYCStateChangedFromTo userId (usrAptoKYCStatus orig) kycStatus
    | usrAptoKYCStatus orig /= Just kycStatus
    ]
  , orig { usrAptoKYCStatus = Just kycStatus
         , usrUserState     = newUserState
         , usrMsgSource     = mid
         }
  )
 where
  userId       = usrUserID orig
  newUserState = case (usrUserState orig, kycStatus) of
    (UserCreated, _) ->
      error $ "Can't set KycState for UserCreated " <> show userId
    (UserWaitingOnPII, _) ->
      error $ "Can't set KycState for UserWaitingOnPII " <> show userId
    (UserWaitingOnKYC   , Passed            ) -> UserWaitingOnKYC -- UserActive is set by setUserState to gaurantee cardholder id is there
    (UserWaitingOnKYC   , AutoVerifyFailed _) -> UserKYCDelay
    (UserWaitingOnKYC   , Rejected _        ) -> UserClosed KYCFailed
    (UserKYCDelay       , Passed            ) -> UserKYCDelay -- UserActive is set by setUserState 
    (UserKYCDelay       , Rejected _        ) -> UserClosed KYCFailed
    (UserKYCDelay       , AutoVerifyFailed _) -> UserKYCDelay -- still delayed
    (UserActive         , Passed            ) -> UserActive   -- Keep as active state
    (UserActive         , Rejected _        ) -> UserClosed KYCFailed
    (UserActive         , AutoVerifyFailed _) -> UserUpdatedKYCDelay
    (UserUpdated        , Passed            ) -> UserUpdated -- UserActive is set by setUserState 
    (UserUpdated        , Rejected _        ) -> UserClosed KYCFailed
    (UserUpdated        , AutoVerifyFailed _) -> UserUpdatedKYCDelay
    (UserUpdatedKYCDelay, AutoVerifyFailed _) -> UserUpdatedKYCDelay
    (UserUpdatedKYCDelay, Rejected _        ) -> UserClosed KYCFailed
    (UserUpdatedKYCDelay, Passed            ) -> UserUpdatedKYCDelay -- UserActive is set by setUserState 
    (UserClosed r       , _                 ) -> UserClosed r

-- inline brittany config for width
-- brittany-next-binding --columns 500
updateAptoCardState :: UTCTime -> AptoCardId -> CardStatus -> UserModel -> FSMUserResult
updateAptoCardState now cid newStatus orig = ([], orig { usrAptoCardId = Just cid, usrAptoCardStatus = Just newStatus, usrCardCreatedOn = Just now })

updatePassword :: Password -> UserModel -> FSMUserResult
updatePassword password model =
  ( [EventUserPasswordChanged (usrUserID model)]
  , model { usrPassword = Just password }
  )

removeFunding :: Maybe PaymentFailureCode -> UserModel -> FSMUserResult
removeFunding reason model =
  let sourceInfo = BankFunding { accountName = usrBankAccountName model
                               , bankName    = usrBankName model
                               , sourceId    = usrDwollaFundingId model
                               }
      userId = usrUserID model
  in  ( [EventUserFSRemoved userId sourceInfo reason]
      , model { usrDwollaFundingId    = Nothing
              , usrBankName           = Nothing
              , usrBankAccountName    = Nothing
              , usrBankAcount         = Nothing
              , usrBankRouting        = Nothing
              , usrBankVerified       = Nothing
              , usrBankVerifedAmounts = Nothing
              , usrBankType           = Nothing
              }
      )

newtype ABARouting = ABARouting Text deriving (Eq, Show)
newtype DDANumber = DDANumber Text deriving (Eq, Show)
newtype AccountName = AccountName Text deriving (Eq, Show)
newtype BankName = BankName Text deriving (Eq, Show)

addNewFunding
  :: ABARouting
  -> DDANumber
  -> AccountName
  -> BankName
  -> AccountType
  -> [Currency]
  -> Bool
  -> UserModel
  -> FSMUserResult
addNewFunding (ABARouting abaRouting) (DDANumber ddaNumber) (AccountName accountName) (BankName bName) accountType veriAmounts isVerified model
  = let
      newModel = model
        { usrBankRouting        = Just . RedactedText $ abaRouting
        , usrBankAcount         = Just . RedactedText $ ddaNumber
        , usrBankVerified       = Just isVerified
        , usrBankVerifedAmounts = Just $ fmap
                                    (fromRational . getMonetaryValue)
                                    veriAmounts
        , usrBankAccountName    = Just accountName
        , usrBankName           = Just bName
        , usrBankType           = Just accountType
        }
      bankInfo = ACHFundingSource { achBankName            = bName
                                  , achAccountName         = accountName
                                  , achABARouting          = abaRouting
                                  , achDDANumber           = ddaNumber
                                  , achVerified            = isVerified
                                  , achVerificationAmounts = veriAmounts
                                  }
      userId = usrUserID newModel
    in
      ([EventUserFSAdded userId bankInfo], newModel)

addManualFunding
  :: ABARouting
  -> DDANumber
  -> AccountName
  -> [Double]
  -> UserModel
  -> FSMUserResult
addManualFunding (ABARouting abaRouting) (DDANumber ddaNumber) (AccountName accountName) amts model
  = let newModel = model { usrBankRouting = Just . RedactedText $ abaRouting
                         , usrBankAcount = Just . RedactedText $ ddaNumber
                         , usrBankVerified = Just False
                         , usrBankVerifedAmounts = Just amts
                         , usrBankAccountName = Just accountName
                         }
        bankInfo = ACHFundingSource
          { achBankName = fromMaybe "Unknown Bank" $ usrBankName model
          , achAccountName         = accountName
          , achABARouting          = abaRouting
          , achDDANumber           = ddaNumber
          , achVerified            = False
          , achVerificationAmounts = fmap (Currency "USD" . toRational) amts
          }
        userId = usrUserID newModel
    in  ([EventUserFSAdded userId bankInfo], newModel)

verifyManualFunding :: Bool -> UserModel -> FSMUserResult
verifyManualFunding verified model =
  ([], model { usrBankVerified = Just verified })

increaseUserRevision
  :: MessageID -> ([UserEvent], UserModel) -> ([UserEvent], UserModel)
increaseUserRevision mid (es, m) =
  (es, m { usrRevision = usrRevision m + 1, usrMsgSource = mid })

closeAccount :: ClosureReason -> UserModel -> FSMUserResult
closeAccount reason model = ([], model { usrUserState = UserClosed reason })

acceptDisclosure
  :: UTCTime -> MessageID -> UserModel -> ([UserEvent], UserModel)
acceptDisclosure now mid model@UserModel{} =
  ([], model { usrDislcosureOk = Just now, usrMsgSource = mid })

acceptConsent :: UTCTime -> MessageID -> UserModel -> ([UserEvent], UserModel)
acceptConsent now mid model@UserModel {..} =
  ( [EventUserInfoChanged usrUserID usrUserState [LegalConsent]]
  , model { usrConstentOk   = Just now
          , usrDislcosureOk = Just now
          , usrMsgSource    = mid
          }
  )

verifyEmailAddresss :: FSMUserResult -> FSMUserResult
verifyEmailAddresss (evts, model@UserModel { usrUserID = userId }) =
  (evts <> [EventUserEmailVerified userId], model { usrEmailVerified = True })

verifyPhoneNumber :: FSMUserResult -> FSMUserResult
verifyPhoneNumber (evts, model@UserModel { usrUserID = userId }) =
  (evts <> [EventUserPhoneVerified userId], model { usrPhoneVerified = True })

setUserDates
  :: UTCTime
  -> UserModel
  -> ([UserEvent], UserModel)
  -> ([UserEvent], UserModel)
setUserDates now orig (evts, newModel) = (evts, chain newModel)
 where
  chain :: UserModel -> UserModel
  chain =
    setCardActivatedDate now orig
      . setCardCeatedOn now orig
      . setUserBecameActiveDate now orig
      . setFirstSigninDate now orig

setFirstSigninDate :: UTCTime -> UserModel -> UserModel -> UserModel
setFirstSigninDate now UserModel { usrPassword = Nothing } newM@UserModel { usrPassword = Just _ }
  = newM { usrFirstSignIn = Just now }
setFirstSigninDate now UserModel { usrUserState = UserCreated } newM@UserModel { usrUserState = UserWaitingOnPII }
  = newM { usrFirstSignIn = Just now }
setFirstSigninDate _ _ newM = newM

setUserBecameActiveDate :: UTCTime -> UserModel -> UserModel -> UserModel
setUserBecameActiveDate _ UserModel { usrUserState = UserActive } newM = newM
setUserBecameActiveDate now UserModel { usrUserState = UserWaitingOnKYC } newM@UserModel { usrUserState = UserActive }
  = newM { usrBecameActiveOn = Just now }
setUserBecameActiveDate now UserModel { usrUserState = UserKYCDelay } newM@UserModel { usrUserState = UserActive }
  = newM { usrBecameActiveOn = Just now }
setUserBecameActiveDate _ _ newM = newM

setCardCeatedOn :: UTCTime -> UserModel -> UserModel -> UserModel
setCardCeatedOn now UserModel { usrAptoCardStatus = Nothing } newM@UserModel { usrAptoCardStatus = Just _ }
  = newM { usrCardCreatedOn = Just now }
setCardCeatedOn _ _ newM = newM

setCardActivatedDate :: UTCTime -> UserModel -> UserModel -> UserModel
setCardActivatedDate _ UserModel { usrAptoCardStatus = Just CardActive } newM =
  newM
setCardActivatedDate now UserModel { usrAptoCardStatus = Just CardCreated } newM@UserModel { usrAptoCardStatus = Just CardActive }
  = newM { usrCardActivatedOn = Just now }
setCardActivatedDate now UserModel { usrAptoCardStatus = Nothing } newM@UserModel { usrAptoCardStatus = Just CardActive }
  = newM { usrCardActivatedOn = Just now }
setCardActivatedDate _ _ newM = newM

changeEmail :: EmailAddress -> UserModel -> ([UserEvent], UserModel)
changeEmail email orig =
  ( [EventUserEmailChanged (usrUserID orig)]
  , orig { usrEmail = normalizeEmail email, usrEmailVerified = False }
  )
