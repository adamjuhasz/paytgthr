{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module AFSM.Web.Event.ProcessUserEvents
  ( processUserEvent
  , createCardForUser
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.FSM.User                  ( ACHFundingSource(..)
                                                , UserEvent(..)
                                                )
import           AFSM.IO.Random                 ( HasRandom(..) )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasCognitoClient    ( HasCognitoClient )
import           AFSM.Monad.HasDecryption       ( HasDecryption )
import           AFSM.Monad.HasEventTracking    ( HasEventTracking(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasIssuerClient    as Issuer
                                                ( CardCreateBody(..)
                                                , HasIssuerClient(..)
                                                , TraceContext
                                                )
import           AFSM.Monad.HasPaymentAuthClient
                                                ( HasPaymentAuthClient )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Change.Card         as CCard
                                                ( CardUpdate(..)
                                                , changeCardState
                                                )
import           AFSM.User.Change.Cardholder    ( getCardHolderId )
import           AFSM.User.Change.KYC           ( changeKYCState
                                                , runKYC
                                                )
import           AFSM.User.Change.Ledger        ( createFundingSourceLedger
                                                , createPayTgthrLedger
                                                )
import           AFSM.Web.Admin.SyncSegment     ( syncGroupState )
import           AFSM.Web.User.Change.Invite    ( createInvite )
import           Control.Monad                  ( when )
import           Control.Monad.Catch            ( MonadCatch )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Either                    ( rights )
import           Data.Text                      ( Text )
import           Shared.Console
import           Shared.Models.Card             ( CardDesign(..)
                                                , CardModel(..)
                                                , CardStatus(..)
                                                , IssuerPlatform(..)
                                                )
import           Shared.Models.Ids              ( CardId(..)
                                                , UserID
                                                )
import           Shared.Models.KYC              ( KycStatus(Passed) )
import           Shared.Models.KYCAssesment     ( KYCAssesment
                                                  ( KYCAssesment
                                                  , kycPassed
                                                  )
                                                )
import           Shared.Models.User             ( FundingInformation
                                                , UserChanges(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.ApiDwolla.Client ( CreateFSBody(..)
                                                , CreateFSResponse(..)
                                                , HasDwollaClient(..)
                                                )
import           Shared.WebAPI.General.API      ( traceToMID )
import           Shared.WebAPI.General.Issuer   ( CardCreatedResponse(..) )

processUserEvent
  :: (CanProcessUserEvents m) => TraceContext -> UserEvent -> m ()
-- | UserWasCreated
processUserEvent trace (EventUserCreated userId) = do
  tracePrint trace "EventUserCreated " userId
  trackUser userId
  return ()
-- | User needs to pass KYC, send to Apto
processUserEvent trace (EventUserStateChanged userId UserWaitingOnKYC) = do
  tracePrint trace "EventUserStateChanged UserWaitingOnKYC " userId
  trackUser userId
  evts <- retryFn trace "kycVerification" $ kycVerification trace userId
  mapM_ (processUserEvent trace) evts
-- | User passed KYC, create card
processUserEvent trace (EventUserStateChanged userId UserActive) = do
  tracePrint trace "EventUserStateChanged UserActive " userId

  trackEvent userId "User Active"
  trackUser userId
  syncGroupState userId

  -- Internal actions
  evtsJournal <- retryFn trace "createPayTgthrJournal"
    $ createPTJournal trace userId
  paEvents <- retryFn trace "createFSJournal"
    $ createFSJournal trace userId "none"
  _ <- retryFn trace "createInvite" $ createInvite trace userId

  -- External actions
  evtsDwolla <- retryFn trace "createFSAccount" $ createFSAccount trace userId

  evtsCardholder <- retryFn trace "createIssuerAccount"
    $ createIssuerAccount trace userId

  evtsApto <- retryFn
    trace
    "createCardForUser"
    (getUserById userId >>= \case
      Nothing -> return []
      Just UserModel { usrPrivacyAcctToken = Nothing } -> do
        traceError trace "Can't create card no privacy token" userId
        return []
      Just UserModel { usrPrivacyAcctToken = Just _ } -> do
        cards <- getCardsFor trace userId
        let cardIsPrivacy CardModel { cardPlatform = PayWithPrivacy _ } = True
            cardIsPrivacy CardModel { cardPlatform = AptoPayments _ }   = False
        let privacyCards = filter cardIsPrivacy cards
        case privacyCards of
          []                   -> createCardForUser trace userId DigitalWallet
          (CardModel {..} : _) -> do
            tracePrint
              trace
              "EventUserStateChanged UserActive not creating a card, exists already"
              (userId, cardId)
            return []
    )
  let allEvents =
        evtsJournal <> paEvents <> evtsCardholder <> evtsApto <> evtsDwolla
  mapM_ (processUserEvent trace) allEvents
-- | User is closed, close their cards
processUserEvent trace (EventUserStateChanged userId (UserClosed closeReason))
  = do
    tracePrint trace
               "EventUserStateChanged UserClosed "
               (userId, UserClosed closeReason)

    trackEventWithProps userId "User Closed"
      $ object ["closeReason" .= closeReason]
    trackUser userId
    syncGroupState userId

    evts <- retryFn trace "closeUsersCard" $ closeUsersCard trace userId

    mapM_ (processUserEvent trace) evts
-- | Nothing to do on these state changes
processUserEvent trace (EventUserStateChanged userId UserKYCDelay) = do
  tracePrint trace "EventUserStateChanged UserKYCDelay " (userId, trace)

  user <- getUserById userId
  case user of
    Nothing -> return ()
    Just UserModel { usrAptoKYCStatus = kycStatus } ->
      trackEventWithProps userId "User KYCDelay"
        $ object ["kycStatus" .= kycStatus]
  trackUser userId

  return ()
processUserEvent trace (EventUserStateChanged userId UserUpdatedKYCDelay) = do
  tracePrint trace "EventUserStateChanged UserUpdatedKYCDelay " (userId, trace)

  user <- getUserById userId
  case user of
    Nothing -> return ()
    Just UserModel { usrAptoKYCStatus = kycStatus } ->
      trackEventWithProps userId "User KYCDelay"
        $ object ["kycStatus" .= kycStatus]
  trackUser userId

  return ()
processUserEvent trace (EventUserStateChanged userId newState) = do
  tracePrint trace "EventUserStateChanged " (userId, newState)
  trackUser userId
  return ()
processUserEvent trace (EventUserInfoChanged userId currState changes) = do
  tracePrint trace "EventUserInfoChanged " (userId, currState, changes)
  trackUser userId

  when (UsersName `elem` changes)    (trackEvent userId "User Changed Name")
  when (UsersAddress `elem` changes) (trackEvent userId "User Changed Address")
  when (UsersPhone `elem` changes)   (trackEvent userId "User Changed Phone")
processUserEvent trace (EventUserCardStateChangedFromTo pStatus card) = do
  tracePrint trace "EventUserCardStateChangedFromTo " (pStatus, card)

  let props = object
        [ "design" .= cardDesign card
        , "lastFour" .= cardLastFour card
        , "previousState" .= pStatus
        , "status" .= cardStatus card
        , "memo" .= cardMemo card
        , "createdAt" .= createdAt card
        , "activatedAt" .= activatedAt card
        , "updatedAt" .= updatedAt card
        ]

  case (pStatus, cardStatus card) of
    (CardUserFrozen, CardActive) ->
      trackEventWithProps (cardholder card) "DebitCard Unfrozen" props
    (_, CardActive) ->
      trackEventWithProps (cardholder card) "DebitCard Activated" props
    (_, CardUserFrozen) ->
      trackEventWithProps (cardholder card) "DebitCard Frozen" props
    (_, CardAdminFrozen) ->
      trackEventWithProps (cardholder card) "DebitCard AdminFrozen" props
    (_, CardClosed) ->
      trackEventWithProps (cardholder card) "DebitCard Closed" props
    (_, _) -> return ()

  evts <- informIssuerOfStateChange trace pStatus card
  mapM_ (processUserEvent trace) evts
processUserEvent trace (EventUserFSAdded userId bankInfo) = do
  tracePrint trace "EventUserFSAdded " userId

  trackUser userId
  syncGroupState userId
  trackEvent userId "FundingSource Added"

  dwollaFsId <- createFSFundingId trace userId bankInfo
  paEvents   <- createFSJournal trace userId dwollaFsId

  mapM_ (processUserEvent trace) paEvents
processUserEvent _trace (EventUserFSVerified userId _) = do
  trackUser userId
  syncGroupState userId
  trackEvent userId "FundingSource Verified"
  return ()
processUserEvent trace (EventUserFSRemoved userId fsInfo _) = do
  trackUser userId
  syncGroupState userId
  trackEvent userId "FundingSource Removed"
  evts <- removeFSFundingId trace userId fsInfo
  mapM_ (processUserEvent trace) evts
processUserEvent _trace (EventUserPasswordChanged userId) = do
  trackUser userId
  trackEvent userId "User Changed Password"
  return ()
processUserEvent _trace (EventUserEmailChanged userId) = do
  trackUser userId
  trackEvent userId "User Changed Email"
  return ()
processUserEvent _trace (EventUserKYCStateChangedFromTo userId _ _) = do
  trackUser userId
  return ()
processUserEvent _trace (EventUserEmailVerified userId) = do
  trackEvent userId "User Verified Email"
  trackUser userId
processUserEvent _trace (EventUserPhoneVerified userId) = do
  trackEvent userId "User Verified Phone"
  trackUser userId
processUserEvent _trace (EventUserCardCreated userId card) = do
  trackEvent userId "User Card Created"
  let props = object
        [ "design" .= cardDesign card
        , "lastFour" .= cardLastFour card
        , "status" .= cardStatus card
        , "memo" .= cardMemo card
        ]
  trackEventWithProps (cardholder card) "DebitCard Created" props
  trackUser userId

kycVerification
  :: ( HasGetUserDB m
     , HasSaveUserDB m
     , GetCurrentTime m
     , HasCognitoClient m
     , HasDecryption m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> m [UserEvent]
kycVerification trace userId = do
  tracePrint
    trace
    "AFSM.Web.Event.ProcessUserEvents.kycVerification Sending user for KYC verification "
    (userId, trace)

  -- see if we already have a KYC status
  previousAssessments <- getUsersKYCAssessments trace userId

  (assessment, evts)  <- case previousAssessments of
    -- None, Send to cognito
    [] -> runKYC trace userId
    -- Last one passed
    a@KYCAssesment { kycPassed = True } : _ -> do
      tracePrint trace "Debug: Short-circuit KYC For " userId
      evts <- changeKYCState trace userId Passed
      return (a, evts)
    -- Last one failed
    a@KYCAssesment { kycPassed = False } : _ -> do
      tracePrint trace "Debug: Short-circuit KYC For " userId
      evts <- changeKYCState trace userId Passed
      return (a, evts)

  tracePrint
    trace
    "AFSM.Web.Event.ProcessUserEvents.kycVerification Completed runKYC "
    (userId, assessment, evts, trace)

  return evts

closeUsersCard
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
closeUsersCard trace userId = do
  tracePrint trace "Closing card for user " (userId, trace)

  let newState = CardClosed

  cards <- getCardsFor trace userId
  let closeThisCard CardModel {..}
        | -- already in this state, move on
          cardStatus == newState = do
          tracePrint trace
                     "closeUsersCard: Not Changing state of card "
                     (userId, cardId, cardPlatform, cardStatus)
          return $ Right []
        | otherwise = CCard.changeCardState userId
                                            trace
                                            cardPlatform
                                            (CardAdminChange CardClosed)
  cardEvents <- concat . rights <$> mapM closeThisCard cards

  tracePrint trace
             "Completed closing cards for user "
             (userId, fmap cardId cards, cardEvents, trace)

  return cardEvents

createCardForUser
  :: ( HasGetUserDB m
     , HasSaveUserDB m
     , GetCurrentTime m
     , HasIssuerClient m
     , HasRandom m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> CardDesign
  -> m [UserEvent]
createCardForUser trace userId requestedCardType = do
  let mid = traceToMID trace

  cardUUID <- CardId <$> getUUID

  tracePrint trace "createCardForUser " (userId, cardUUID, requestedCardType)

  let cardOptions = CardCreateBody { newCardId     = cardUUID
                                   , newCardDesign = requestedCardType
                                   , newCardMemo   = Nothing
                                   }
  createdCard <- createCard trace userId cardOptions

  let platform = createdCardId createdCard

  tracePrint trace
             "createCardForUser Completed creating card for user "
             (userId, cardUUID, requestedCardType, cardOptions, createdCard)

  now <- getCurrentTime
  let newCard = CardModel
        { cardId       = cardUUID
        , cardPlatform = platform
        , cardRevision = 1
        , cardDesign   = requestedCardType
        , cardholder   = userId
        , cardStatus   = createdStatus createdCard
        , cardMemo     = createdCardMemo createdCard
        , createdAt    = now
        , activatedAt  = case createdStatus createdCard of
                           CardCreated     -> Nothing
                           CardActive      -> Just now
                           CardUserFrozen  -> Nothing
                           CardAdminFrozen -> Nothing
                           CardClosed      -> Nothing
        , closedAt     = case createdStatus createdCard of
                           CardCreated     -> Nothing
                           CardActive      -> Nothing
                           CardUserFrozen  -> Nothing
                           CardAdminFrozen -> Nothing
                           CardClosed      -> Just now
        , updatedAt    = now
        , cardLastFour = createdLastFour createdCard
        }

  saveCardModel trace newCard
  let creatEvents = [EventUserCardCreated userId newCard]

  let activateCardNow = case requestedCardType of
        PinkToYellow    -> False
        YellowToPink    -> False
        Virtual         -> True
        DigitalWallet   -> True
        PhysicalBlack   -> False
        UnknownDesign _ -> False

  tracePrint trace
             "createCardForUser activateCardNow activateCardNow "
             (userId, requestedCardType, activateCardNow)

  evts <- if activateCardNow
    then do
      res <- CCard.changeCardState userId trace platform
        $ CardStateUpdate CardActive
      case res of
        Left  _  -> return []
        Right es -> return es
    else return []

  tracePrint trace "Completed changeCardState " (userId, evts, mid)

  return $ creatEvents <> evts

createFSAccount
  :: (HasGetUserDB m, HasDwollaClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
createFSAccount trace uid = do
  user <- getUserById uid
  case user of
    Nothing -> return []
    Just UserModel { usrDwollaId = Nothing } -> do
      tracePrint trace "createFSAccount: Creating dwolla accoutn for" uid
      _ <- createDwollaAccount trace uid
      return []
    Just UserModel { usrDwollaId = Just _ } -> do
      tracePrint trace "createFSAccount: Already has dwolla account" uid
      return []

type DwollaFSId = Text
createFSFundingId
  :: (HasDwollaClient m)
  => TraceContext
  -> UserID
  -> ACHFundingSource
  -> m DwollaFSId
createFSFundingId trace uid ACHFundingSource {..} = do

  CreateFSResponse {..} <- createFundingSource
    trace
    uid
    CreateFSBody { abaRoutingNo = achABARouting
                 , ddaAccountNo = achDDANumber
                 , accountName  = achAccountName
                 , bankName     = achBankName
                 }

  -- Dwolla updats the "dwollafunding" field
  return dwollaFSId

removeFSFundingId
  :: (HasDwollaClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> FundingInformation
  -> m [UserEvent]
removeFSFundingId trace uid fsInfo = do
  tracePrint trace "removeFSFundingId " uid
  removeFundingSource trace uid fsInfo
  return []

informIssuerOfStateChange
  :: (HasIssuerClient m)
  => TraceContext
  -> CardStatus
  -> CardModel
  -> m [UserEvent]
informIssuerOfStateChange _ _ CardModel { cardPlatform = AptoPayments _ } =
  return []
informIssuerOfStateChange trace _ crd@CardModel { cardPlatform = PayWithPrivacy _ }
  = do
    Issuer.changeCardState trace crd
    return []

createIssuerAccount
  :: ( HasGetUserDB m
     , HasSaveUserDB m
     , GetCurrentTime m
     , MonadIO m
     , HasIssuerClient m
     )
  => TraceContext
  -> UserID
  -> m [UserEvent]
createIssuerAccount trace userId = do
  (action, evts) <- getCardHolderId trace userId
  tracePrint trace "createIssuerAccount results: " (userId, action, evts)
  return evts

createPTJournal
  :: (HasPaymentAuthClient m, HasGetUserDB m, MonadIO m, MonadCatch m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
createPTJournal trace uid = do
  createPayTgthrLedger trace uid

createFSJournal
  :: (HasPaymentAuthClient m, HasGetUserDB m, MonadIO m, MonadCatch m)
  => TraceContext
  -> UserID
  -> Text
  -> m [UserEvent]
createFSJournal trace uid fsId = do
  createFundingSourceLedger trace uid fsId
