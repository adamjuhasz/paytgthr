{-# LANGUAGE RecordWildCards #-}

module AFSM.User.Change.KYC where

import           AFSM.Cognito.Client           as Cognito
                                                ( AddressComparison(..)
                                                , BirthComparison(..)
                                                , Comparison(..)
                                                , CreateProfileBody
                                                  ( CreateProfileBody
                                                  )
                                                , CreateProfileResponse
                                                  ( CreateProfileResponse
                                                  )
                                                , IdentityAssessmentBody
                                                  ( IdentityAssessmentBody
                                                  )
                                                , IdentityAssessmentResponse(..)
                                                , IdentityRecordComparison(..)
                                                , IdentitySearchBody
                                                  ( IdentitySearchBody
                                                  , address
                                                  , dob
                                                  , name
                                                  , phone
                                                  , profileId
                                                  , ssn
                                                  )
                                                , IdentitySearchResponse
                                                  ( IdentitySearchResponse
                                                  , searchId
                                                  )
                                                , NameComparison(..)
                                                , PhoneComparison(..)
                                                , Routes(..)
                                                , SSNComparison(..)
                                                , ScreeningBody(..)
                                                , ScreeningResponse(..)
                                                , ScreeningStatus(..)
                                                , asClientM
                                                , ofacProgramID
                                                )
import           AFSM.FSM.User                  ( UserEvent
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserDates
                                                , setUserState
                                                , updateKYCState
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasCognitoClient    ( HasCognitoClient(..) )
import           AFSM.Monad.HasDecryption       ( CipherText(CipherText)
                                                , HasDecryption(..)
                                                , PlainText(PlainText)
                                                )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Control.Monad.Zip              ( MonadZip(mzip) )
import           Data.Foldable                  ( asum )
import           Data.Function                  ( (&) )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                , mapMaybe
                                                )
import           Data.Ord                       ( Down(Down) )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.KYC              ( KYCFailureReasons(..)
                                                , KycStatus(..)
                                                )
import           Shared.Models.KYCAssesment    as KYC
                                                ( CognitoProfileId(..)
                                                , IdentitySearchId
                                                , KYCAssesment(..)
                                                )
import           Shared.Models.User             ( PhoneNumber(..)
                                                , RedactedText(..)
                                                , UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

changeKYCState
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> KycStatus
  -> m [UserEvent]
changeKYCState trace userId kycStatus = do
  let mid = traceToMID trace
  tracePrint trace "changeKYCState " (userId, kycStatus)

  now  <- getCurrentTime
  user <- fromJust <$> getUserById userId

  let (events, newModel) =
        user
          & updateKYCState mid kycStatus
          & setUserState
          & setUserDates now user
          & sendStateChangeEvents user
          & increaseUserRevision mid

  tracePrint trace
             "changeKYCState "
             (userId, kycStatus, updateKYCState mid kycStatus user)

  -- print debug diff
  diffUser trace user newModel

  _ <- saveUserModel newModel
  return events

mzip4
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Text, Text, Text, Text)
mzip4 (Just a) (Just b) (Just c) (Just d) = Just (a, b, c, d)
mzip4 _        _        _        _        = Nothing

unPhone :: PhoneNumber -> Text
unPhone (PhoneNumber t) = t

runKYC
  :: ( HasGetUserDB m
     , MonadIO m
     , HasCognitoClient m
     , HasDecryption m
     , GetCurrentTime m
     , HasSaveUserDB m
     )
  => TraceContext
  -> UserID
  -> m (KYCAssesment, [UserEvent])
runKYC trace userId = do
  userM        <- getUserById userId
  searchParams <- case userM of
    Just UserModel { usrPhone = Just (PhoneNumber phone), ..} -> do
      plainTextSSN <- case usrSSN of
        Just (RedactedText t) -> do
          (PlainText decrypted) <- decryptSSN userId $ CipherText t
          return $ Just decrypted
        Nothing -> return Nothing

      return IdentitySearchBody
        { profileId = CognitoProfileId ""
        , phone     = "+1" <> phone
        , ssn       = plainTextSSN
        , name      = mzip usrFirstName usrLastName
        , dob       = usrDOB
        , address   = mzip4 usrAddressStreet
                            usrAddressCity
                            usrAddressState
                            usrAddressZip
        }
    Just u  -> error $ "Error: user missing some traits for idcheck " <> show u
    Nothing -> error $ "Error: user does not exist " <> show userId

  screeningParams <- case userM of
    Just UserModel { usrDOB = Just dob, usrFirstName = Just firstName, usrLastName = Just lastName }
      -> return ScreeningBody { programId         = ofacProgramID
                              , fullName          = firstName <> " " <> lastName
                              , dateOfBirth       = dob
                              , location          = "US"
                              , customerReference = userId
                              }
    Just u  -> error $ "Error: user missing some traits for ofac " <> show u
    Nothing -> error $ "Error: user does not exist " <> show userId

  ofacRes <- runCognitoRoute trace $ _CreateScreening asClientM screeningParams
  ScreeningResponse _ screenResult <- case ofacRes of
    Left e -> do
      traceError trace "Error: _CreateScreening " (userId, e)
      error $ "_CreateScreening " <> show e
    Right r -> return r

  createRes <- runCognitoRoute trace
    $ _CreateProfile asClientM CreateProfileBody
  (CreateProfileResponse newProfileId _ _) <- case createRes of
    Left e -> do
      traceError trace "Error: _CreateProfile " (userId, e)
      error $ "_CreateProfile " <> show e
    Right r -> return r

  let searchToRun = searchParams { Cognito.profileId = newProfileId }

  searchRes <- runCognitoRoute trace $ _IdentitySearch asClientM searchToRun
  IdentitySearchResponse { searchId = newSearchId } <- case searchRes of
    Left  e -> error $ "_IdentitySearch " <> show (userId, e)
    Right r -> return r

  assesmentRes <-
    runCognitoRoute trace
    $ _IdentityAssessment asClientM
    $ IdentityAssessmentBody newSearchId

  now       <- getCurrentTime
  assesment <- case assesmentRes of
    Left  e        -> error $ "Error: _IdentityAssessment " <> show (userId, e)
    Right response -> return $ processAssesment now
                                                userId
                                                newProfileId
                                                newSearchId
                                                screenResult
                                                response

  let filledAssessment = assesment
        { KYC.inputPhone   = unPhone <$> (userM >>= usrPhone)
        , KYC.inputSSN     = userM >>= usrSSN
        , KYC.inputName    = (userM >>= usrFirstName)
                             <> Just " "
                             <> (userM >>= usrLastName)
        , KYC.inputAddress = (userM >>= usrAddressStreet)
                             <> Just ", "
                             <> (userM >>= usrAddressCity)
                             <> Just ", "
                             <> (userM >>= usrAddressState)
                             <> Just " "
                             <> (userM >>= usrAddressZip)
        }

  saveAssesment trace filledAssessment

  evts <- if kycPassed filledAssessment
    then changeKYCState trace userId Passed
    else changeKYCState trace userId $ AutoVerifyFailed $ failureReasons
      filledAssessment

  return (filledAssessment, evts)

pullIdentityRecord :: Comparison -> Maybe IdentityRecordComparison
pullIdentityRecord (IdentityRecord r) = Just r
pullIdentityRecord _                  = Nothing

pullPhoneComparison :: Comparison -> Maybe PhoneComparison
pullPhoneComparison (Phone x) = Just x
pullPhoneComparison _         = Nothing

pullAddressComparison :: Comparison -> Maybe AddressComparison
pullAddressComparison (Address x) = Just x
pullAddressComparison _           = Nothing

pullSSNComparison :: Comparison -> Maybe SSNComparison
pullSSNComparison (SSN x) = Just x
pullSSNComparison _       = Nothing

pullNameComparison :: Comparison -> Maybe NameComparison
pullNameComparison (Name x) = Just x
pullNameComparison _        = Nothing

pullDOBComparison :: Comparison -> Maybe BirthComparison
pullDOBComparison (Birth x) = Just x
pullDOBComparison _         = Nothing

processAssesment
  :: UTCTime
  -> UserID
  -> CognitoProfileId
  -> IdentitySearchId
  -> ScreeningStatus
  -> IdentityAssessmentResponse
  -> KYCAssesment
processAssesment now uid profId searchId screenResult IdentityAssessmentResponse {..}
  = let
      highestMatch =
        listToMaybe
          . sortOn (Down . identityScore)
          . mapMaybe pullIdentityRecord
          $ included
      highestScore = maybe 0 identityScore highestMatch
      -- Phone
      phones =
        sortOn (Down . phoneScore) . mapMaybe pullPhoneComparison $ included
      highstPhone = phoneScore <$> listToMaybe phones
      -- Address
      addresses =
        sortOn (Down . addressScore) . mapMaybe pullAddressComparison $ included
      highestAddress = addressScore <$> listToMaybe addresses
      -- SSN
      ssns = sortOn (Down . ssnScore) . mapMaybe pullSSNComparison $ included
      highestSSN = ssnScore <$> listToMaybe ssns
      -- DOB
      dobs = sortOn (Down . birthScore) . mapMaybe pullDOBComparison $ included
      highestDOB = birthScore <$> listToMaybe dobs
      dobYear =
        asum
          [ listToMaybe dobs >>= sourceDOBYear
          , listToMaybe dobs >>= inputDOBYear
          ]
      names =
        sortOn (Down . nameScore) . mapMaybe pullNameComparison $ included
      highestName = nameScore <$> listToMaybe names
      failureReasons =
        [ AddressScoreLow | highestAddress < Just 10 ]
          <> [ PhoneScoreLow | highstPhone < Just 0 ]
          <> [ SSNScoreLow | highestSSN /= Just 100 ]
          <> [ NameScoreLow | highestName < Just 90 ]
          <> [ DOBScoreLow | highestDOB < Just 50 ]
          <> [ IdentityTheftRisk | dobYear <= Just 1976 ]
          <> [ WatchlistNeedsReview | screenResult == ScreeningPendingReview ]
          <> [ WatchlistRejected | screenResult == ScreeningRejected ]
      passed = null failureReasons
    in
      KYCAssesment
        { userId         = uid
        , profileId      = profId
        , searchId       = searchId
        , kycPassed      = passed
        , scoreOverall   = highestScore
        , failureReasons = failureReasons
        , createdAt      = now
        -- Average scores for each trait
        , scorePhone     = highstPhone
        , scoreAddress   = highestAddress
        , scoreSSN       = highestSSN
        , scoreName      = highestName
        -- Count of results for each trait
        , countPhone     = length phones
        , countAddresss  = length addresses
        , countSSN       = length ssns
        , countName      = length names
        -- Inputs we sent to the search
        , inputPhone     = Nothing
        , inputAddress   = Nothing
        , inputSSN       = Nothing
        , inputName      = Nothing
        -- rawResults
        , sourcePhone    = fmap
                             (\(PhoneComparison _ score source _) ->
                               (source, score)
                             )
                             phones
        , sourceAddress  = fmap
                             (\(AddressComparison _ score source _) ->
                               (source, score)
                             )
                             addresses
        , sourceSSN      = fmap
                             (\(SSNComparison _ score source _) ->
                               (RedactedText source, score)
                             )
                             ssns
        , sourceName     = fmap
                             (\(NameComparison _ score source _) -> (source, score))
                             names
        }
