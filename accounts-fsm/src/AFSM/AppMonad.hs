{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE ConstraintKinds #-}

module AFSM.AppMonad where

import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.DB.GroupGet               ( getGroupCatSplits
                                                , getGroupDefaultSplit
                                                )
import           AFSM.DB.GroupSave              ( saveACategorySplit )
import qualified AFSM.DB.Rewards               as RewardsDB
import           AFSM.DB.Tokens                 ( HasTokenDB(..) )
import           AFSM.DB.UserGet                ( findACard
                                                , getAccountByCard
                                                , getAccountById
                                                , getAccountsWithBankInfo
                                                , getAllAccountsIDs
                                                , getAllAssesmentsForUser
                                                , getCardModelByUserId
                                                , getLastUserNote
                                                , getSpecficCard
                                                )
import           AFSM.DB.UserSave               ( saveAKYCAssesment
                                                , saveAUserNote
                                                , saveCard
                                                , saveUserDeviceIP
                                                )
import           AFSM.IO.Random                 ( HasRandom(..) )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasCognitoClient    ( HasCognitoClient(..) )
import           AFSM.Monad.HasDecryption       ( CipherText(CipherText)
                                                , HasDecryption(..)
                                                , PlainText
                                                )
import           AFSM.Monad.HasEnvironment      ( HasEnvironment(..)
                                                , RunningEnvironment(Production)
                                                )
import           AFSM.Monad.HasEventTracking    ( HasEventTracking(..) )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasInviteCode       ( HasInviteCode(..) )
import           AFSM.Monad.HasIssuerClient     ( HasIssuerClient(..) )
import           AFSM.Monad.HasPaymentAuthClient
                                                ( HasPaymentAuthClient(..) )
import           AFSM.Monad.HasReferralDB       ( HasReferralDB(..) )
import           AFSM.Monad.HasRewardsDB        ( HasRewardsDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           Control.Concurrent             ( MVar )
import           Control.Concurrent.Async       ( async )
import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadMask
                                                , MonadThrow(..)
                                                )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , asks
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Data.Aeson
import           Data.Maybe                     ( fromJust )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Clock
import qualified Data.UUID                     as U
import           Data.UUID.V4                   ( nextRandom )
import           GHC.Stack                      ( HasCallStack )
import           Network.HTTP.Client            ( Manager
                                                , Request
                                                )
import           Servant                        ( Handler
                                                , NoContent(NoContent)
                                                , ServerError
                                                )
import           Servant.Client
import           Shared.Console                 ( tracePrint )
import           Shared.DB.PartnerInvite        ( getInviteForUser
                                                , getInviteFromCode
                                                , saveAnInvite
                                                )
import           Shared.DB.Referral            as RDB
                                                ( getReferralCode
                                                , getReferralCodeForUser
                                                , getReferralProgram
                                                , getReferralProgressOf
                                                , getReferreeProgressFor
                                                , setReferralCode
                                                , setReferralProgram
                                                , setReferralProgress
                                                )
import           Shared.Database                ( PooledDB )
import           Shared.Models.Card             ( CardModel(..)
                                                , CardStatus(..)
                                                , IssuerPlatform(..)
                                                )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids              ( JournalId(JournalId)
                                                , ReferralProgramID(..)
                                                , UserID(UserID)
                                                )
import           Shared.Models.Referral.ReferralCode
                                                ( ReferralCodeDisplay )
import           Shared.Models.User             ( FundingInformation(..)
                                                , RedactedText(RedactedText)
                                                , UserModel(usrRevision, usrSSN)
                                                )
import           Shared.Track.HasTracing        ( HasTracing(..)
                                                , trackSpan
                                                )
import qualified Shared.Track.Segment          as Segment
import           Shared.Track.Willow
import           Shared.WebAPI.ApiApto.Client  as Apto
                                                ( Routes(_CardClose)
                                                , aptoRoutes
                                                )
import           Shared.WebAPI.ApiDwolla.API   as Dwolla
                                                ( RemoveFSBody(..)
                                                , Routes(..)
                                                )
import           Shared.WebAPI.ApiDwolla.Client ( HasDwollaClient(..)
                                                , dwollaRoutes
                                                )
import           Shared.WebAPI.ApiPrivacy.Client
                                               as Privacy
                                                ( Routes(..)
                                                , privacyIO
                                                )
import           Shared.WebAPI.General.API      ( incrementTrace )
import           Shared.WebAPI.General.Issuer   ( CreateCardholderAction(..) )
import           Shared.WebAPI.PaymentAuth.API as PA
                                                ( CreateLedgerJournalBody(..)
                                                , CreateLedgerTrxBody(..)
                                                , Routes(..)
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( paymentauthRoutes )
import           System.Random                  ( randomRIO )
import           Web.JWT                        ( Signer )

type CanProcessUserEvents m
  = ( HasGetUserDB m
    , HasSaveUserDB m
    , HasGetGroupDB m
    , GetCurrentTime m
    , HasIssuerClient m
    , HasDwollaClient m
    , HasRandom m
    , HasEnvironment m
    , HasDecryption m
    , HasEventTracking m
    , HasCognitoClient m
    , HasPaymentAuthClient m
    , HasInviteCode m
    , MonadIO m
    , MonadMask m
    , HasCallStack
    )

type CanProcessGroupEvents m
  = ( HasGetGroupDB m
    , HasSaveGroupDB m
    , MonadIO m
    , GetCurrentTime m
    , HasEventTracking m
    , HasGetUserDB m
    , HasRewardsDB m
    , HasRandom m
    , MonadMask m
    , HasCallStack
    )

data AppConfig = AppConfig
  { dbActions              :: DBActions -- lazy for testing
  , aptoEnv                :: ClientEnv -- lazy for testing
  , privacyEnv             :: ClientEnv -- lazy for testing
  , dwollaEnv              :: ClientEnv -- lazy for testing
  , paymentAuthEnv         :: ClientEnv -- lazy for testing
  , dbPool                 :: PooledDB  -- lazy for testing
  , segmentRequest         :: Request -> IO () -- lazy for testing
  , environment            :: RunningEnvironment  -- lazy for testing
  , stackDriver            :: (Manager, (Text, Signer, Text)) -- lazy for testing
  , cognitoEnv             :: ClientEnv -- lazy for testing
  , convergentSSNDecrypter :: CipherText -> IO PlainText
  , convergentSSNEncrypter :: PlainText -> IO CipherText
  , onewaySSNDecrypter     :: CipherText -> IO PlainText
  , isShuttingDown         :: MVar Bool
  , willowEnv              :: ClientEnv -- lazy for testing
  }

newtype AppWebM a =
    AppWebM { unAppWebM :: ReaderT AppConfig Handler a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader AppConfig
      , MonadError ServerError
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

newtype AppIOM a =
    AppIOM { unAppIOM :: ReaderT AppConfig IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader AppConfig
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

instance HasSaveUserDB AppIOM where
  saveUserModel model = do
    DBActions {..} <- asks dbActions
    liftIO $ cSaveUser model
  saveCardModel _trace card = do
    pool <- asks dbPool
    liftIO $ withResource pool $ saveCard card
  saveAssesment _trace ass = do
    pool <- asks dbPool
    liftIO $ withResource pool $ saveAKYCAssesment ass
  saveUserNote _trace uid note = do
    pool <- asks dbPool
    liftIO $ withResource pool $ saveAUserNote uid note
  saveDeviceIP _trace uid ip = do
    pool <- asks dbPool
    liftIO $ withResource pool $ saveUserDeviceIP uid ip

instance HasSaveUserDB AppWebM where
  saveUserModel model = do
    DBActions {..} <- asks dbActions
    liftIO $ cSaveUser model
  saveCardModel parentTrace card = do
    pool  <- asks dbPool
    trace <- incrementTrace parentTrace
    traceSpan "AFSM.Monad.HasSaveUserDB.saveCard" parentTrace trace
      $ liftIO
      $ withResource pool
      $ saveCard card
  saveAssesment parentTrace ass = do
    pool  <- asks dbPool
    trace <- incrementTrace parentTrace
    traceSpan "AFSM.Monad.HasSaveUserDB.saveAKYCAssesment" parentTrace trace
      $ liftIO
      $ withResource pool
      $ saveAKYCAssesment ass
  saveUserNote parentTrace uid note = do
    pool  <- asks dbPool
    trace <- incrementTrace parentTrace
    traceSpan "AFSM.Monad.HasSaveUserDB.saveAUserNote" parentTrace trace
      $ liftIO
      $ withResource pool
      $ saveAUserNote uid note
  saveDeviceIP parentTrace uid ip = do
    pool  <- asks dbPool
    trace <- incrementTrace parentTrace
    traceSpan "AFSM.Monad.HasSaveUserDB.saveUserDeviceIP" parentTrace trace
      $ liftIO
      $ withResource pool
      $ saveUserDeviceIP uid ip

-- HasGetUserDB

instance HasGetUserDB AppIOM where
  getUserByEmail email = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccountByEmail email
  getUserByPhone phone = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccountByPhone phone
  getUserById uid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccountById uid
  getUserByCardholder cid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccounByCardholder cid
  getUserByBank routing account = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetUsersWithAcountNumber routing account
  getAllActiveUsers = do
    DBActions {..} <- asks dbActions
    liftIO cGetAllActiveUsers
  getUsersWithSSN ssn = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetUsersWithSSN ssn
  getCardsFor _trace userId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getCardModelByUserId userId
  getUserByCard _trace cardId = do
    pool    <- asks dbPool
    account <- liftIO $ withResource pool $ getAccountByCard cardId
    case account of
      Nothing -> return Nothing
      Just a  -> liftIO $ withResource pool $ getAccountById a
  getCard _trace cardId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getSpecficCard cardId
  getAllUsers states = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getAllAccountsIDs states
  getUsersKYCAssessments trace userId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getAllAssesmentsForUser trace userId
  findCard _trace issuerId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ findACard issuerId
  getUserNote _trace uid = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getLastUserNote uid

instance HasGetUserDB AppWebM where
  getUserByEmail email = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccountByEmail email
  getUserByPhone phone = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccountByPhone phone
  getUserById uid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccountById uid
  getUserByCardholder cid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetAccounByCardholder cid
  getUserByBank routing account = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getAccountsWithBankInfo routing account
  getAllActiveUsers = do
    DBActions {..} <- asks dbActions
    liftIO cGetAllActiveUsers
  getUsersWithSSN ssn = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetUsersWithSSN ssn
  getCardsFor _trace userId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getCardModelByUserId userId
  getUserByCard _trace cardId = do
    pool    <- asks dbPool
    account <- liftIO $ withResource pool $ getAccountByCard cardId
    case account of
      Nothing -> return Nothing
      Just a  -> liftIO $ withResource pool $ getAccountById a
  getCard _trace cardId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getSpecficCard cardId
  getAllUsers states = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getAllAccountsIDs states
  getUsersKYCAssessments trace userId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getAllAssesmentsForUser trace userId
  findCard _trace issuerId = do
    pool <- asks dbPool
    liftIO $ withResource pool $ findACard issuerId
  getUserNote parentTrace uid = do
    pool  <- asks dbPool
    trace <- incrementTrace parentTrace
    traceSpan "AFSM.Monad.HasGetUserDB.getLastUserNote" parentTrace trace
      $ liftIO
      $ withResource pool
      $ getLastUserNote uid

-- GetCurrentTime

instance GetCurrentTime AppIOM  where
  getCurrentTime = liftIO Clock.getCurrentTime

instance GetCurrentTime AppWebM where
  getCurrentTime = liftIO Clock.getCurrentTime

-- HasGetGroupDB

instance HasGetGroupDB AppIOM where
  getGroupByGroupId gid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetGroupById gid
  getGroupsForUserFiltered allowed uid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetGroupsByUserIdFiltered allowed uid
  getGroupsForUser uid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetGroupsByUserId uid
  getGroupMainSplit gid = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getGroupDefaultSplit gid
  getGroupSplits gid = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getGroupCatSplits gid

instance HasGetGroupDB AppWebM where
  getGroupByGroupId gid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetGroupById gid
  getGroupsForUserFiltered allowed uid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetGroupsByUserIdFiltered allowed uid
  getGroupsForUser uid = do
    DBActions {..} <- asks dbActions
    liftIO $ cGetGroupsByUserId uid
  getGroupMainSplit gid = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getGroupDefaultSplit gid
  getGroupSplits gid = do
    pool <- asks dbPool
    liftIO $ withResource pool $ getGroupCatSplits gid

-- HasSaveGroupDB

instance HasSaveGroupDB AppIOM where
  saveGroupModel model = do
    DBActions {..} <- asks dbActions
    liftIO $ cSaveGroup model
  saveCategorySplit cat = do
    pool <- asks dbPool
    liftIO $ withResource pool $ saveACategorySplit cat

instance HasSaveGroupDB AppWebM  where
  saveGroupModel model = do
    DBActions {..} <- asks dbActions
    liftIO $ cSaveGroup model
  saveCategorySplit cat = do
    pool <- asks dbPool
    liftIO $ withResource pool $ saveACategorySplit cat

-- HasTokenDB

instance HasTokenDB AppIOM where
  getUserTokenWithCode code user = do
    DBActions {..} <- asks dbActions
    liftIO $ cFindToken code user
  saveUserToken token = do
    DBActions {..} <- asks dbActions
    _              <- liftIO $ cSaveToken token
    return ()

instance HasTokenDB AppWebM where
  getUserTokenWithCode code user = do
    DBActions {..} <- asks dbActions
    liftIO $ cFindToken code user
  saveUserToken token = do
    DBActions { cSaveToken = save } <- asks dbActions
    _                               <- liftIO $ save token
    return ()

instance HasIssuerClient AppWebM where
  activateCard trace uid _ = do
    liftIO $ putStr "Error: activateCard for privacy called" >> print
      (uid, trace)
    error "activateCard not needed for Privacy"
  createCardholder trace uid = do
    env    <- asks privacyEnv
    trace' <- incrementTrace trace
    let fn = _CardholderCreate $ privacyIO env
    traceSpan "AFSM.Monad.HasIssuerClient.createCardholder" trace trace'
      $ liftIO
      $ fn trace' uid
  updateCardholder trace uid = do
    liftIO $ putStr "Error: updateCardholder for privacy called" >> print
      (uid, trace)
    return CardholderNotUpdated
  pingIssuerService = do
    env <- asks privacyEnv
    _   <- liftIO $ Privacy._health $ privacyIO env
    return ()
  createCard trace uid body = do
    env    <- asks privacyEnv
    trace' <- incrementTrace trace
    let fn = _CardCreate $ privacyIO env
    traceSpan "AFSM.Monad.HasIssuerClient.createCard" trace trace' $ liftIO $ fn
      trace'
      uid
      body
  changeCardState trace card = do
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasIssuerClient.changeCardState" trace trace'
      $ case (cardStatus card, cardPlatform card) of
          (CardClosed, PayWithPrivacy token) -> do
            env <- asks privacyEnv
            let fn = Privacy._CardClose $ privacyIO env
            NoContent <- liftIO $ fn trace' (cardholder card) token
            return ()
          (CardClosed, AptoPayments _) -> do
            env <- asks aptoEnv
            let fn = Apto._CardClose $ aptoRoutes env
            NoContent <- liftIO $ fn trace' (cardholder card)
            return ()
          (_        , AptoPayments _      ) -> return ()
          (newStatus, PayWithPrivacy token) -> do
            env <- asks privacyEnv
            let fn = Privacy._CardChangeState $ privacyIO env
            NoContent <- liftIO $ fn trace' (cardholder card) token newStatus
            return ()
  setCardPin trace card body = do
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasIssuerClient.setCardPin" trace trace'
      $ case cardPlatform card of
          PayWithPrivacy token -> do
            env <- asks privacyEnv
            let fn = Privacy._CardChangePin $ privacyIO env
            NoContent <- liftIO $ fn trace' (cardholder card) token body
            return ()
          AptoPayments _ -> error "No Apto cards anymore"

instance HasDwollaClient AppWebM where
  pingDwolla = do
    env <- asks dwollaEnv
    _   <- liftIO $ Dwolla._health $ dwollaRoutes env
    return ()
  createDwollaAccount trace uid = do
    env    <- asks dwollaEnv
    trace' <- incrementTrace trace
    let fn = Dwolla._AccountCreate $ dwollaRoutes env
    liftIO $ fn trace' uid
  createFundingSource trace uid body = do
    env    <- asks dwollaEnv
    trace' <- incrementTrace trace
    let fn = Dwolla._FundingSourceCreate $ dwollaRoutes env
    liftIO $ fn trace' uid body
  initiatePayment trace pid = do
    env    <- asks dwollaEnv
    trace' <- incrementTrace trace
    let fn = Dwolla._PaymentInitiate $ dwollaRoutes env
    NoContent <- liftIO $ fn trace' pid
    return ()
  cancelPaynent trace pid = do
    env    <- asks dwollaEnv
    trace' <- incrementTrace trace
    let fn = Dwolla._PaymentInitiate $ dwollaRoutes env
    NoContent <- liftIO $ fn trace' pid
    return ()
  removeFundingSource trace uid fs@BankFunding { sourceId = Nothing } = do
    liftIO $ putStr "Error: Removing fs without id" >> print (uid, fs, trace)
    return ()
  removeFundingSource trace uid BankFunding { sourceId = Just fsSourceId } = do
    env    <- asks dwollaEnv
    trace' <- incrementTrace trace
    let fn = Dwolla._FundingSourceRemove $ dwollaRoutes env
    NoContent <- liftIO $ fn trace' uid $ RemoveFSBody fsSourceId
    return ()

instance HasPaymentAuthClient AppWebM where
  pingPaymentAuth = do
    env <- asks paymentAuthEnv
    _   <- liftIO $ PA._health $ paymentauthRoutes env
    return ()
  getSpendableBalance trace uid = do
    env    <- asks paymentAuthEnv
    trace' <- incrementTrace trace
    let fn = PA._GetSpendableBalance $ paymentauthRoutes env
    liftIO $ fn trace' uid
  makeVerificationPayment trace uid body = do
    env    <- asks paymentAuthEnv
    trace' <- incrementTrace trace
    let fn = PA._MakeVerificationPayment $ paymentauthRoutes env
    NoContent <- liftIO $ fn trace' uid body
    return ()
  getPayment trace pid = do
    env    <- asks paymentAuthEnv
    trace' <- incrementTrace trace
    let fn = PA._GetPayment $ paymentauthRoutes env
    liftIO $ fn trace' pid
  updatePayment trace pid updates = do
    env    <- asks paymentAuthEnv
    trace' <- incrementTrace trace
    let fn = PA._UpdatePayment $ paymentauthRoutes env
    NoContent <- liftIO $ fn trace' pid updates
    return ()
  createLedger trace jType name = do
    jid    <- JournalId <$> liftIO nextRandom
    env    <- asks paymentAuthEnv
    trace' <- incrementTrace trace

    let body = CreateLedgerJournalBody { newJournalId   = jid
                                       , newJournalType = jType
                                       , newJournalName = name
                                       , startBalance   = Currency "USD" 0
                                       }
    let fn = PA._CreateJournal $ paymentauthRoutes env
    _ <- liftIO $ fn trace' body
    return ()
  getLedger trace search = do
    env    <- asks paymentAuthEnv
    trace' <- incrementTrace trace
    let fn = PA._GetJournal $ paymentauthRoutes env
    liftIO $ fn trace' search
  journalTransfer trace from to fact = do
    env    <- asks paymentAuthEnv
    trace' <- incrementTrace trace
    let fn = PA._CreateJournalTrx $ paymentauthRoutes env
    let
      body =
        CreateLedgerTrxBody { fromJournal = from, toJournal = to, fact = fact }
    NoContent <- liftIO $ fn trace' body
    return ()

generateCode :: IO Text
generateCode = do
  l1 <- randomRIO ('A', 'Z')
  l2 <- randomRIO ('A', 'Z')
  l3 <- randomRIO ('A', 'Z')
  n1 <- randomRIO (0 :: Int, 9)
  n2 <- randomRIO (0 :: Int, 9)
  n3 <- randomRIO (0 :: Int, 9)

  return $ T.pack ([l1] <> [l2] <> [l3] <> show n1 <> show n2 <> show n3)

generateRefCode :: IO ReferralCodeDisplay
generateRefCode = do
  l2 <- randomRIO ('A', 'Z')
  l3 <- randomRIO ('A', 'Z')
  n1 <- randomRIO (0 :: Int, 9)
  n2 <- randomRIO (0 :: Int, 9)

  return $ T.pack (['R'] <> show n1 <> show n2 <> [l2] <> [l3])

instance HasRandom AppIOM where
  getUUID               = liftIO nextRandom
  getRandomInviteCode   = liftIO generateCode
  getRandomReferralCode = liftIO generateRefCode

instance HasRandom AppWebM where
  getUUID               = liftIO nextRandom
  getRandomInviteCode   = liftIO generateCode
  getRandomReferralCode = liftIO generateRefCode

instance HasEventTracking AppIOM where
  trackEventWithProps userId eventName props
    | userId == UserID U.nil = return ()
    | otherwise = do
      segmentRequester <- asks segmentRequest
      userModelMaybe   <- getUserById userId
      case userModelMaybe of
        Nothing ->
          liftIO
            $  putStr "Error: Could not get user inside trackEventWithProps "
            >> print (userId, eventName)
        Just u -> liftIO . void . async $ Segment.trackEvent segmentRequester
                                                             u
                                                             eventName
                                                             props

  trackEvent userId eventName
    | userId == UserID U.nil = return ()
    | otherwise              = trackEventWithProps userId eventName $ object []
  trackUser userId = do
    segmentRequester <- asks segmentRequest
    userModelMaybe   <- getUserById userId
    case userModelMaybe of
      Nothing -> liftIO $ do
        putStr "Error: User is Empty for trackUser " >> print userId
        return ()
      Just u -> liftIO $ do
        putStr "Updating Segment about " >> print userId
        void . async $ Segment.sendUserUpdate segmentRequester u
  trackOneOffTrait userId traits
    | userId == UserID U.nil = return ()
    | otherwise = do
      segmentRequester <- asks segmentRequest
      liftIO . void . async $ Segment.trackOneOffTrait segmentRequester
                                                       userId
                                                       traits
  trackEventToCustomerIO _ _ _ = return ()

instance HasEventTracking AppWebM where
  trackEventWithProps userId eventName props = do
    segmentRequester <- asks segmentRequest
    userModelMaybe   <- getUserById userId
    case userModelMaybe of
      Nothing ->
        liftIO
          $  putStr "Error: Could not get user inside trackEventWithProps "
          >> print (userId, eventName)
      Just u -> liftIO . void . async $ Segment.trackEvent segmentRequester
                                                           u
                                                           eventName
                                                           props
  trackEvent userId eventName =
    trackEventWithProps userId eventName $ object []
  trackUser userId = do
    segmentRequester <- asks segmentRequest
    userModelMaybe   <- getUserById userId
    willowE          <- asks willowEnv
    case userModelMaybe of
      Nothing -> liftIO $ do
        putStr "Error: User is Empty for trackUser " >> print userId
        return ()
      Just u -> liftIO $ do
        putStr "Updating Segment about " >> print userId
        void . async $ Segment.sendUserUpdate segmentRequester u
        void . async $ do
          let willowUpdate = _UpdateAttribute
                willowClientM
                "Bearer aaaaaaabbbbbbbcd"
                (AttributeUpdate "user"
                                 (T.pack . show $ userId)
                                 (toJSON u)
                                 [("user", T.pack . show $ userId)]
                )
          void $ runClientM willowUpdate willowE
  trackOneOffTrait userId traits = do
    segmentRequester <- asks segmentRequest
    liftIO . void . async $ Segment.trackOneOffTrait segmentRequester
                                                     userId
                                                     traits
    willowE <- asks willowEnv
    liftIO . void . async $ do
      let willowUpdate = _UpdateAttribute
            willowClientM
            "Bearer aaaaaaabbbbbbbcd"
            (AttributeUpdate
              "user"
              (T.pack . show $ userId)
              (object $ foldr (\(t, v) accum -> accum <> [t .= v]) [] traits)
              []
            )
      void $ runClientM willowUpdate willowE
  trackEventToCustomerIO _ _ _ = return ()

instance HasEnvironment AppWebM where
  getEnvironment = asks environment

instance HasTracing AppWebM where
  traceChildSpan name parentTrace fn = do
    thisTrace <- incrementTrace parentTrace
    traceSpan name parentTrace thisTrace fn
  traceSpan name parentTrace thisTrace fn = do
    tracePrint parentTrace "Tracing: " name

    startTime      <- getCurrentTime
    res            <- fn
    endTime        <- getCurrentTime

    env            <- getEnvironment

    (manager, jwt) <- asks stackDriver
    when
      (env == Production)
      (void . liftIO . async $ trackSpan manager
                                         jwt
                                         name
                                         parentTrace
                                         thisTrace
                                         startTime
                                         endTime
      )
    --return result
    return res

instance HasCognitoClient AppWebM where
  runCognitoRoute trace fn = do
    env    <- asks cognitoEnv
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasCognitoClient.runCognitoRoute" trace trace'
      $ liftIO
      $ runClientM fn env

instance HasDecryption AppWebM where
  decryptSSN userId cipher = do
    decrypter       <- asks convergentSSNDecrypter
    onewayDecrypter <- asks onewaySSNDecrypter
    encrypter       <- asks convergentSSNEncrypter

    res             <- liftIO $ do
      decrypted <- try $ decrypter cipher
      case decrypted of
        Right t -> return $ Right t
        -- Try old decryption
        Left (convergentError :: SomeException) -> do
          oneway <- try $ onewayDecrypter cipher
          case oneway of
            Left (onewayError :: SomeException) ->
              error
                $  "Error: HasDecryption decryptSSN could not decrypt "
                <> show (userId, cipher, onewayError, convergentError)
            Right anSSN -> do
              putStr "Converted SSN for " >> print userId
              reEncrypted <- encrypter anSSN
              return $ Left reEncrypted
    case res of
      Right t                    -> return t
      Left  enc@(CipherText raw) -> do
        user <- getUserById userId
        case user of
          Nothing ->
            error
              $  "Error: HasDecryption decryptSSN could not find "
              <> show userId
          Just u -> do
            saveUserModel $ u { usrSSN      = Just $ RedactedText raw
                              , usrRevision = usrRevision u + 1
                              }
            liftIO $ decrypter enc

instance HasInviteCode AppWebM where
  getUsersInvite trace uid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasInviteCode.getUsersInvite" trace trace'
      $ liftIO
      $ withResource pool
      $ getInviteForUser uid
  getInvite trace code = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasInviteCode.getInvite" trace trace'
      $ liftIO
      $ withResource pool
      $ getInviteFromCode code
  saveInvite trace invite = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasInviteCode.saveInvite" trace trace'
      $ liftIO
      $ withResource pool
      $ saveAnInvite invite

instance HasRewardsDB AppWebM where
  getGroupsCurrentRewards trace gid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getGroupsCurrentRewards" trace trace'
      $ liftIO
      $ withResource pool
      $ RewardsDB.groupGetRewards gid
  saveBoostActivation trace act = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.saveBoostActivation" trace trace'
      $ liftIO
      $ withResource pool
      $ RewardsDB.saveRewardBoostActivation act
  getGroupsActivations trace gid states = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getGroupsActivations" trace trace'
      $ liftIO
      $ withResource pool
      $ RewardsDB.getRewardBoostActivations gid states
  getAllActiveRewards trace = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getAllActiveRewards" trace trace'
      $ liftIO
      $ withResource pool RewardsDB.getActiveRewards
  saveRewardBoost trace boost = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.saveRewardBoost" trace trace'
      $ liftIO
      $ withResource pool
      $ RewardsDB.saveRewardBoost boost
  getRewardBoost trace rid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getRewardBoost" trace trace'
      $ liftIO
      $ withResource pool
      $ RewardsDB.getRewardBoost rid
  getAllRewardsEver trace = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getAllRewardsEver" trace trace'
      $ liftIO
      $ withResource pool RewardsDB.getAllRewards
  getBoostActivation trace rid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getBoostActivation" trace trace'
      $ liftIO
      $ withResource pool (RewardsDB.getBoostActivation rid)

-- #### New Section

instance HasReferralDB AppWebM where
  getPublicReferralProgram trace = do
    let pid = ReferralProgramID $ fromJust $ U.fromText
          "00000000-0000-0000-0000-000000000000"
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasReferralDB.getPublicReferralProgram" trace trace'
      $ liftIO
      $ withResource pool (RDB.getReferralProgram pid)

  saveReferralProgram trace program = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasReferralDB.saveReferralProgram" trace trace'
      $ liftIO
      $ withResource pool (setReferralProgram program)

  getReferralProgram trace pid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getReferralProgram" trace trace'
      $ liftIO
      $ withResource pool (RDB.getReferralProgram pid)

  getReferralCodeForUID trace uid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getReferralCodeForUID" trace trace'
      $ liftIO
      $ withResource pool (RDB.getReferralCodeForUser uid)

  getReferralCode trace code = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getReferralCode" trace trace'
      $ liftIO
      $ withResource pool (RDB.getReferralCode code)

  saveReferralCode trace code = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.saveReferralCode" trace trace'
      $ liftIO
      $ withResource pool (RDB.setReferralCode code)

  getReferralProgressFor trace uid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getReferralProgressFor" trace trace'
      $ liftIO
      $ withResource pool (RDB.getReferralProgressOf uid)

  getReferreeProgressFor trace uid = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.getReferreeProgressFor" trace trace'
      $ liftIO
      $ withResource pool (RDB.getReferreeProgressFor uid)

  saveReferralProgress trace progress = do
    pool   <- asks dbPool
    trace' <- incrementTrace trace
    traceSpan "AFSM.Monad.HasRewardsDB.saveReferralProgress" trace trace'
      $ liftIO
      $ withResource pool (RDB.setReferralProgress progress)
