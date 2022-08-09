{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
--Servant
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AFSM.Cognito.Client
  ( module AFSM.Cognito.Client
  , Routes(..)
  , CreateProfileBody(..)
  , IdentitySearchBody(..)
  , IdentityAssessmentBody(..)
  , CreateProfileResponse(..)
  , IdentitySearchResponse(..)
  , IdentityAssessmentResponse(..)
  , Comparison(..)
  , CognitoProfileId(..)
  ) where

import           Crypto.Hash                    ( Digest
                                                , hash
                                                )
import           Crypto.Hash.Algorithms         ( SHA256 )
import           Crypto.MAC.HMAC                ( HMAC(hmacGetDigest)
                                                , hmac
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (<?>)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , Object
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                , withText
                                                )
import           Data.Aeson.Types               ( JSONPathElement(Key) )
import           Data.ByteArray.Encoding        ( Base(Base64)
                                                , convertToBase
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Char8         as BC8
import qualified Data.ByteString.Lazy          as LBS
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( asum )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                , parseTimeM
                                                )
import           Data.UUID                      ( nil )
import           Network.HTTP.Client           as Client
                                                ( BodyReader
                                                , Manager
                                                , ManagerSettings(..)
                                                , Request(..)
                                                , RequestBody(..)
                                                , Response(..)
                                                )
import           Network.HTTP.Client.TLS        ( newTlsManagerWith
                                                , tlsManagerSettings
                                                )
import           Network.HTTP.Types             ( HeaderName
                                                , RequestHeaders
                                                )
import           Servant
import           Servant.API.Generic
import           Servant.Client                 ( ClientEnv
                                                , ClientM
                                                , runClientM
                                                )
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                )
import           Shared.Models.KYCAssesment     ( CognitoProfileId(..)
                                                , IdentitySearchId(..)
                                                )
import           Shared.Models.User             ( UserID(..) )

data CreateProfileBody = CreateProfileBody
  deriving (Eq, Show)
instance ToJSON CreateProfileBody where
  toJSON CreateProfileBody = object ["data" .= object [("type", "profile")]]

data CreateProfileResponse = CreateProfileResponse
  { profileId          :: CognitoProfileId
  , createdAt          :: UTCTime
  , identitySearchLink :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON CreateProfileResponse where
  parseJSON = withObject "CreateProfileResponse" $ \o -> do
    dataObj              <- o .: "data"
    responseType :: Text <- dataObj .: "type" <?> Key "data"
    profileId            <- dataObj .: "id" <?> Key "data"
    attributes           <- dataObj .: "attributes" <?> Key "data"
    createdAt            <- attributes .: "created_at" <?> Key "attributes"
    relationships        <- dataObj .: "relationships" <?> Key "data"
    identitySearches     <- relationships .: "identity_searches" <?> Key
      "relationships"
    links <- identitySearches .: "links" <?> Key "identity_searches"
    identitySearchLink <- links .: "related" <?> Key "links"

    case responseType of
      "profile" -> return ()
      t -> fail $ "CreateProfileResponse doesn't recognize type " <> show t

    return CreateProfileResponse { .. }

data IdentitySearchBody = IdentitySearchBody
  { profileId :: CognitoProfileId
  , phone     :: Text
  , ssn       :: Maybe Text
  , name      :: Maybe (Text, Text)
  , dob       :: Maybe UTCTime
  , address   :: Maybe (Text, Text, Text, Text)
  }
  deriving (Eq, Show, Generic)
instance ToJSON IdentitySearchBody where
  toJSON IdentitySearchBody {..} =
    let
      phoneAttribute = ["phone" .= object ["number" .= phone]]
      ssnAttribute   = case ssn of
        Nothing -> []
        Just ssnT ->
          [ "ssn" .= object
              [ "area" .= T.take 3 ssnT
              , "group" .= (T.takeEnd 2 . T.take 5 $ ssnT)
              , "serial" .= T.takeEnd 4 ssnT
              ]
          ]

      nameAttribute = case name of
        Nothing -> []
        Just (fname, lname) ->
          ["name" .= object ["first" .= fname, "last" .= lname]]
      dobAttribute = case dob of
        Nothing -> []
        Just dobTime ->
          [ "birth" .= object
              [ "day"
                .= (read $ formatTime defaultTimeLocale "%d" dobTime :: Int)
              , "month"
                .= (read $ formatTime defaultTimeLocale "%m" dobTime :: Int)
              , "year"
                .= (read $ formatTime defaultTimeLocale "%0Y" dobTime :: Int)
              ]
          ]
      addressAttribute = case address of
        Nothing -> []
        Just (street, city, state, zipcode) ->
          [ "us_address" .= object
              [ "street" .= street
              , "city" .= city
              , "subdivision" .= state
              , "postal_code" .= zipcode
              ]
          ]
    in
      object
        [ "data" .= object
            [ ("type", "identity_search")
            , "attributes" .= object
              (  phoneAttribute
              <> ssnAttribute
              <> nameAttribute
              <> dobAttribute
              <> addressAttribute
              )
            , "relationships" .= object
              [ "profile" .= object
                  ["data" .= object [("type", "profile"), "id" .= profileId]]
              ]
            ]
        ]

data IdentitySearchResponse = IdentitySearchResponse
  { searchId  :: IdentitySearchId
  , profileId :: CognitoProfileId
  }
  deriving (Eq, Show, Generic)
instance FromJSON IdentitySearchResponse where
  parseJSON = withObject "IdentitySearchResponse" $ \o -> do
    dataObj       <- o .: "data"

    searchId      <- dataObj .: "id" <?> Key "data"

    relationships <- dataObj .: "relationships" <?> Key "data"
    relProfile    <- relationships .: "profile" <?> Key "data.relationships"
    profileData   <- relProfile .: "data" <?> Key "data.relationships.profile"
    profileId     <- CognitoProfileId <$> profileData .: "id" <?> Key
      "data.relationships.profile.data"

    return IdentitySearchResponse { .. }

newtype IdentityAssessmentBody = IdentityAssessmentBody
  { searchId :: IdentitySearchId
  }
  deriving (Eq, Show)
instance ToJSON IdentityAssessmentBody where
  toJSON IdentityAssessmentBody {..} = object
    [ "data" .= object
        [ ("type", "identity_assessment")
        , "relationships" .= object
          [ "identity_search" .= object
              ["data" .= object [("type", "identity_search"), "id" .= searchId]]
          ]
        ]
    ]

type ComparisonId = Text

newtype ComparisonRelationship = ComparisonRelationship
  { relId :: ComparisonId
  }
  deriving (Eq, Show)
instance FromJSON ComparisonRelationship where
  parseJSON = withObject "ComparisonLink" $ \o -> do
    relId <- o .: "id"
    return ComparisonRelationship { .. }

newtype ComparisonLinks = ComparisonLinks
  { compId :: [ComparisonRelationship]
  }
  deriving (Eq, Show)
instance FromJSON ComparisonLinks where
  parseJSON = withObject "ComparisonLink" $ \o -> do
    compId <- o .: "data"

    return ComparisonLinks { .. }

data IdentityRecordComparison = IdentityRecordComparison
  { comparisonId  :: ComparisonId
  , identityScore :: Int
  , adddresses    :: Maybe ComparisonLinks
  , phones        :: Maybe ComparisonLinks
  , births        :: Maybe ComparisonLinks
  , names         :: Maybe ComparisonLinks
  , ssns          :: Maybe ComparisonLinks
  }
  deriving (Eq, Show)
instance FromJSON IdentityRecordComparison where
  parseJSON = withObject "IdentityRecordComparison" $ \o -> do
    objType :: Text <- o .: "type"
    case objType of
      "identity_record_comparison" -> return ()
      t ->
        fail $ T.unpack $ "This is not an IdentityRecordComparison but " <> t

    comparisonId                        <- o .: "id"
    attributes                          <- o .: "attributes"
    identityScore <- attributes .: "score" <?> Key "attributes"

    relationships                       <- o .: "relationships"
    adddresses :: Maybe ComparisonLinks <-
      relationships .:? "address_comparisons"
    phones :: Maybe ComparisonLinks <- relationships .:? "phone_comparisons"
    births :: Maybe ComparisonLinks <- relationships .:? "birth_comparisons"
    names :: Maybe ComparisonLinks  <- relationships .:? "name_comparisons"
    ssns :: Maybe ComparisonLinks   <- relationships .:? "ssn_comparisons"

    return IdentityRecordComparison { .. }

data PhoneComparison = PhoneComparison
  { comparisonId :: ComparisonId
  , phoneScore   :: Int
  , sourceNumber :: Text
  , inputNumber  :: Text
  }
  deriving (Eq, Show)
instance FromJSON PhoneComparison where
  parseJSON = withObject "PhoneComparison" $ \o -> do
    objType :: Text <- o .: "type"
    case objType of
      "phone_comparison" -> return ()
      t -> fail $ T.unpack $ "This is not an PhoneComparison but " <> t

    comparisonId <- o .: "id"
    attributes   <- o .: "attributes"
    phoneScore   <- attributes .: "score" <?> Key "attributes"
    components   <- attributes .: "components" <?> Key "attributes"
    compNumbers  <- components .: "number" <?> Key "attributes.components"
    sourceNumber <- compNumbers .: "source" <?> Key
      "attributes.components.number"
    inputNumber <- compNumbers .: "input" <?> Key "attributes.components.number"

    return PhoneComparison { .. }

data SSNComparison = SSNComparison
  { comparisonId :: Text
  , ssnScore     :: Int
  , sourceSSN    :: Text
  , inputSSN     :: Text
  }
  deriving (Eq, Show)
instance FromJSON SSNComparison where
  parseJSON = withObject "SSNComparison" $ \o -> do
    objType :: Text <- o .: "type"
    case objType of
      "ssn_comparison" -> return ()
      t -> fail $ T.unpack $ "This is not an SSNComparison but " <> t

    comparisonId <- o .: "id"
    attributes   <- o .: "attributes"
    ssnScore     <- attributes .: "score" <?> Key "attributes"
    components   <- attributes .: "components" <?> Key "attributes"

    area         <- components .: "area" <?> Key "attributes.components"
    group        <- components .: "group" <?> Key "attributes.components"
    serial       <- components .: "serial" <?> Key "attributes.components"

    sourceArea   <- area .: "source" <?> Key "attributes.components.area"
    sourceGroup  <- group .: "source" <?> Key "attributes.components.group"
    sourceSerial <- serial .: "source" <?> Key "attributes.components.serial"

    inputArea    <- area .: "input" <?> Key "attributes.components.area"
    inputGroup   <- group .: "input" <?> Key "attributes.components.group"
    inputSerial  <- serial .: "input" <?> Key "attributes.components.serial"

    let sourceSSN =
          fromMaybe "🚨" sourceArea
            <> "-"
            <> fromMaybe "🚨" sourceGroup
            <> "-"
            <> fromMaybe "🚨" sourceSerial
    let inputSSN =
          fromMaybe "🚨" inputArea
            <> "-"
            <> fromMaybe "🚨" inputGroup
            <> "-"
            <> fromMaybe "🚨" inputSerial

    return SSNComparison { .. }

data NameComparison = NameComparison
  { comparisonId :: Text
  , nameScore    :: Int
  , sourceName   :: Text
  , inputName    :: Text
  }
  deriving (Eq, Show)
instance FromJSON NameComparison where
  parseJSON = withObject "NameComparison" $ \o -> do
    objType :: Text <- o .: "type"
    case objType of
      "name_comparison" -> return ()
      t -> fail $ T.unpack $ "This is not an NameComparison but " <> t

    comparisonId <- o .: "id"
    attributes   <- o .: "attributes"
    nameScore    <- attributes .: "score" <?> Key "attributes"
    components   <- attributes .: "components" <?> Key "attributes"

    firstName    <- components .: "first" <?> Key "attributes.components"
    middleName   <- components .: "middle" <?> Key "attributes.components"
    lastName     <- components .: "last" <?> Key "attributes.components"

    sourceFirst  <- firstName .:? "source" <?> Key
      "attributes.components.firstName"
    sourceMiddle <- middleName .:? "source" <?> Key
      "attributes.components.middleName"
    sourceLast <- lastName .:? "source" <?> Key
      "attributes.components.middleName"

    inputFirst <- firstName .:? "input" <?> Key
      "attributes.components.firstName"
    inputMiddle <- middleName .:? "input" <?> Key
      "attributes.components.middleName"
    inputLast <- lastName .:? "input" <?> Key "attributes.components.middleName"

    let sourceName =
          fromMaybe "🚨" sourceFirst
            <> " "
            <> fromMaybe "🚨" sourceMiddle
            <> " "
            <> fromMaybe "🚨" sourceLast

    let inputName =
          fromMaybe "🚨" inputFirst
            <> " "
            <> fromMaybe "🚨" inputMiddle
            <> " "
            <> fromMaybe "🚨" inputLast

    return NameComparison { .. }

data AddressComparison = AddressComparison
  { comparisonId  :: Text
  , addressScore  :: Int
  , sourceAddress :: Text
  , inputAddress  :: Text
  }
  deriving (Eq, Show)
instance FromJSON AddressComparison where
  parseJSON = withObject "AddressComparison" $ \o -> do
    objType :: Text <- o .: "type"
    case objType of
      "us_address_comparison" -> return ()
      t -> fail $ T.unpack $ "This is not an AddressComparison but " <> t

    comparisonId <- o .: "id"
    attributes   <- o .: "attributes"
    addressScore <- attributes .: "score" <?> Key "attributes"
    components   <- attributes .: "components" <?> Key "attributes"

    street       <- components .: "street" <?> Key "attributes.components"
    city         <- components .: "city" <?> Key "attributes.components"
    state        <- components .: "subdivision" <?> Key "attributes.components"
    zipcode      <- components .: "postal_code" <?> Key "attributes.components"

    sourceStreet <- street .:? "source" <?> Key "attributes.components.street"
    sourceCity   <- city .:? "source" <?> Key "attributes.components.city"
    sourceState  <- state .:? "source" <?> Key
      "attributes.components.subdivision"
    sourceZipcode <- zipcode .:? "source" <?> Key
      "attributes.components.postal_code"

    inputStreet  <- street .:? "input" <?> Key "attributes.components.street"
    inputCity    <- city .:? "input" <?> Key "attributes.components.city"
    inputState <- state .:? "input" <?> Key "attributes.components.subdivision"
    inputZipcode <- zipcode .:? "input" <?> Key
      "attributes.components.postal_code"

    let sourceAddress =
          fromMaybe "🚨" sourceStreet
            <> ", "
            <> fromMaybe "🚨" sourceCity
            <> ", "
            <> fromMaybe "🚨" sourceState
            <> " "
            <> fromMaybe "🚨" sourceZipcode

    let inputAddress =
          fromMaybe "🚨" inputStreet
            <> ", "
            <> fromMaybe "🚨" inputCity
            <> ", "
            <> fromMaybe "🚨" inputState
            <> " "
            <> fromMaybe "🚨" inputZipcode

    return AddressComparison { .. }

data BirthComparison = BirthComparison
  { comparisonId  :: ComparisonId
  , birthScore    :: Int
  , sourceDOB     :: Text
  , inputDOB      :: Text
  , sourceDOBYear :: Maybe Int
  , inputDOBYear  :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON BirthComparison where
  parseJSON = withObject "BirthComparison" $ \o -> do
    objType :: Text <- o .: "type"
    case objType of
      "birth_comparison" -> return ()
      t -> fail $ T.unpack $ "This is not an BirthComparison but " <> t

    comparisonId  <- o .: "id"
    attributes    <- o .: "attributes"
    birthScore    <- attributes .: "score" <?> Key "attributes"
    components    <- attributes .: "components" <?> Key "attributes"

    year          <- components .: "year" <?> Key "attributes.components"
    month         <- components .: "month" <?> Key "attributes.components"
    day           <- components .: "day" <?> Key "attributes.components"

    sourceDOBYear <- year .: "source"
    sourceMonth   <- month .: "source"
    sourceDay     <- day .: "source"

    inputDOBYear  <- year .: "input"
    inputMonth    <- month .: "input"
    inputDay      <- day .: "input"

    let showText :: Int -> Text
        showText = T.pack . show

    let sourceDOB =
          maybe "🚨" showText sourceMonth
            <> "/"
            <> maybe "🚨" showText sourceDay
            <> "/"
            <> maybe "🚨" showText sourceDOBYear
    let inputDOB =
          maybe "🚨" showText inputMonth
            <> "/"
            <> maybe "🚨" showText inputDay
            <> "/"
            <> maybe "🚨" showText inputDOBYear

    return BirthComparison { .. }

data Comparison
  = IdentityRecord IdentityRecordComparison
  | Phone PhoneComparison
  | SSN SSNComparison
  | Name NameComparison
  | Address AddressComparison
  | Birth BirthComparison
  deriving (Eq, Show)
instance FromJSON Comparison where
  parseJSON val = asum
    [ IdentityRecord <$> parseJSON val
    , Phone <$> parseJSON val
    , SSN <$> parseJSON val
    , Name <$> parseJSON val
    , Address <$> parseJSON val
    , Birth <$> parseJSON val
    ]

data IdentityAssessmentResponse = IdentityAssessmentResponse
  { assesmentId :: Text
  , included    :: [Comparison]
  }
  deriving (Eq, Show)
instance FromJSON IdentityAssessmentResponse where
  parseJSON = withObject "IdentityAssessmentResponse" $ \o -> do
    dataObj     <- o .: "data"
    assesmentId <- dataObj .: "id" <?> Key "data"

    included    <- o .: "included"

    return IdentityAssessmentResponse { .. }

data IdentitySearchJobResponse = IdentitySearchJobResponse
  { searchId :: Text
  , other    :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type ProgramId = Text

ofacProgramID :: ProgramId
ofacProgramID = "prg_1Hc9z6EZry2Vco"

data ScreeningBody = ScreeningBody
  { programId         :: ProgramId
  , fullName          :: Text
  , dateOfBirth       :: UTCTime
  , location          :: Text
  , customerReference :: UserID
  }
  deriving (Eq, Show)
instance ToJSON ScreeningBody where
  toJSON ScreeningBody {..} = object
    [ "search_terms" .= object
      [ "program_id" .= programId
      , "name" .= fullName
      , "date_of_birth" .= formatTime defaultTimeLocale "%F" dateOfBirth
      , "location" .= location
      ]
    , "customer_reference" .= customerReference
    ]

data ScreeningStatus
  = ScreeningCleared
  | ScreeningRejected
  | ScreeningPendingReview
  deriving(Eq, Show)
instance FromJSON ScreeningStatus where
  parseJSON = withText "ScreeningStatus" $ \case
    "cleared"        -> return ScreeningCleared
    "rejected"       -> return ScreeningRejected
    "pending_review" -> return ScreeningPendingReview
    t                -> error $ "ScreeningStatus failed on " <> show t

type ScreeningId = Text
data ScreeningResponse = ScreeningResponse
  { screeningId     :: ScreeningId
  , screeningStatus :: ScreeningStatus
  }
  deriving (Eq, Show)
instance FromJSON ScreeningResponse where
  parseJSON = withObject "ScreeningResponse" $ \o -> do
    screeningId     <- o .: "id"
    screeningStatus <- o .: "status"

    return ScreeningResponse { .. }

-- type IdentitySearchResult
--   = WithStatus
--       201
--       (Headers '[Header "Content-Location" Text] IdentitySearchResponse)

-- type IdentitySearchJob
--   = WithStatus
--       202
--       (Headers '[Header "Content-Location" Text] IdentitySearchJobResponse)



-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  { _CreateProfile      :: route :- "profiles" :> ReqBody '[JSON] CreateProfileBody :> Post '[JSON] CreateProfileResponse
  , _IdentitySearch     :: route :- "identity_searches" :> ReqBody '[JSON] IdentitySearchBody :> Post '[JSON] IdentitySearchResponse
  , _IdentityAssessment :: route :- "identity_assessments" :> ReqBody '[JSON] IdentityAssessmentBody :> Post '[JSON] IdentityAssessmentResponse
  , _CreateScreening    :: route :- "screenings" :> ReqBody '[JSON] ScreeningBody :> Post '[JSON] ScreeningResponse
  , _ListScreenings     :: route :- "screenings" :> Get '[JSON] Object
  , _ListPrograms       :: route :- "programs" :> Get '[JSON] Object
  }
  deriving Generic

asClientM :: Routes (AsClientT ClientM)
asClientM = genericClient

replaceHeaders :: (Eq a, IsString a) => (HeaderName, a) -> (HeaderName, a)
replaceHeaders (a, "application/vnd.api+json") = (a, "application/json")
replaceHeaders (a, b                         ) = (a, b)

newCognitoManager :: ApiSecret -> ApiKey -> IO Manager
newCognitoManager apiSecret apiKey = newTlsManagerWith $ tlsManagerSettings
  { managerModifyRequest  = \req -> do
    let reqMethod = method req
    let reqPath   = path req
    let body      = requestBody req
    bsBody <- case body of
      RequestBodyBS  bs  -> return bs
      RequestBodyLBS lbs -> return $ LBS.toStrict lbs
      _                  -> do
        putStrLn "cant convert to digest"
        error "cant convert to digest"

    cognitoHeaders <- generateCognitoAuth apiSecret
                                          apiKey
                                          reqMethod
                                          reqPath
                                          bsBody

    let req' = req { requestHeaders = cognitoHeaders }
    return req'
  , managerModifyResponse = \res -> do
    let res' :: Client.Response BodyReader = res
          { responseHeaders = fmap replaceHeaders (Client.responseHeaders res)
          }
    return res'
  }

type ApiSecret = ByteString
type ApiKey = ByteString

generateCognitoAuth
  :: ApiSecret
  -> ApiKey
  -> ByteString
  -> ByteString
  -> ByteString
  -> IO RequestHeaders
generateCognitoAuth apiSecret apiKey method path body = do
  now <- getCurrentTime

  let normalizedMethod = C.map toLower method
  let date =
        BC8.pack (formatTime defaultTimeLocale "%a, %d %b %Y %T" now) <> " GMT"
  let digest = convertToBase Base64 (hash body :: Digest SHA256)
  let target :: ByteString =
        "(request-target): "
          <> normalizedMethod
          <> " "
          <> path
          <> "\ndate: "
          <> date
          <> "\ndigest: SHA-256="
          <> digest
  let authorization :: HMAC SHA256 = hmac apiSecret target

  let base64Auth :: ByteString =
        convertToBase Base64 $ hmacGetDigest authorization
  let
    headers =
      [ ("Date"  , date)
      , ("Digest", "SHA-256=" <> digest)
      , ( "Cognito-Version"
        , case path of
          "/screenings" -> "2020-08-14"
          "/programs"   -> "2020-08-14"
          _             -> "2016-09-01"
        )
      , ( "Authorization"
        , "Signature keyId=\""
          <> apiKey
          <> "\",algorithm=\"hmac-sha256\",headers=\"(request-target) date digest\",signature=\""
          <> base64Auth
          <> "\""
        )
      , ("Content-Type", "application/vnd.api+json")
      ]
  return headers

example :: ClientEnv -> IO ()
example env = do
  let screening = ScreeningBody
        { programId         = ofacProgramID
        , fullName          = "Adam Juhasz"
        , dateOfBirth       = fromJust $ parseTimeM True
                                                    defaultTimeLocale
                                                    "%Y-%-m-%-d"
                                                    "1985-4-02"
        , location          = "US"
        , customerReference = UserID nil
        }
  ofacRes <- runClientM (_CreateScreening asClientM screening) env
  case ofacRes of
    Left e -> do
      print e
      error "_CreateScreening"
    Right r -> do
      putStr "OFAC passed: " >> print r
      return ()

  createRes <- runClientM (_CreateProfile asClientM CreateProfileBody) env
  CreateProfileResponse {..} <- case createRes of
    Left e -> do
      print e
      error "_CreateProfile"
    Right r -> do
      print r
      return r

  let searchBody = IdentitySearchBody
        { profileId = profileId
        , phone     = "+12341231234"
        , ssn       = Just "234123123"
        , name      = Just ("Adam", "Bob")
        , dob       = parseTimeM True defaultTimeLocale "%d/%m/%y" "08/02/1985"
        , address   = Just ("123 min", "san f", "CA", "23343")
        }

  print searchBody

  searchRes <- runClientM (_IdentitySearch asClientM searchBody) env
  print searchRes
  IdentitySearchResponse searchId _ <- case searchRes of
    Left  _ -> error "_IdentitySearch"
    Right r -> return r

  assesmentRes <- runClientM
    (_IdentityAssessment asClientM $ IdentityAssessmentBody searchId)
    env
  print assesmentRes

  return ()
