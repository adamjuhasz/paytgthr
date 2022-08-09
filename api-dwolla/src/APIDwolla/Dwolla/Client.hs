{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module APIDwolla.Dwolla.Client where

import           APIDwolla.Dwolla.Types         ( DwollaEmbeddedError
                                                , DwollaError(..)
                                                , DwollaErrorReason(..)
                                                , DwollaLink(linkHref)
                                                )
import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Crypto.Hash.MD5                ( hash )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , KeyValue(..)
                                                , Object
                                                , eitherDecode
                                                , encode
                                                , object
                                                , withObject
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor                   ( (<&>) )
import           Data.HashMap.Strict            ( empty
                                                , fromList
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( toASCIIBytes )
import           Data.UUID.V4                   ( nextRandom )
import           Network.HTTP.Client            ( HttpException(..)
                                                , HttpExceptionContent(..)
                                                , Manager
                                                , ManagerSettings(..)
                                                , Request(..)
                                                , RequestBody(RequestBodyLBS)
                                                , Response(..)
                                                , applyBasicAuth
                                                , httpLbs
                                                , newManager
                                                , parseRequest
                                                , responseTimeoutMicro
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Types.Method      ( StdMethod(..)
                                                , renderStdMethod
                                                )
import           Network.HTTP.Types.Status      ( statusCode )
import           Shared.Console
import           Shared.Models.Currency         ( getIsoCode
                                                , getMonetaryValue
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentFailureCode(..)
                                                , PaymentId(..)
                                                , PaymentType(..)
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , RedactedText(..)
                                                , UserID(..)
                                                , UserModel(..)
                                                , defaultUser
                                                )
import           Shared.TgthrMessages.Base      ( AccountType(..)
                                                , DepositoryType(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , randomTrace
                                                )

type TokenRes = (DwollaTokenResponse, (Manager, URLPath))
type TokenMaker = Either ClientErrors TokenRes

newtype DwollaToken = DwollaToken ByteString deriving (Eq, Show)
type URLPath = String

-- https://docs.dwolla.com/#application-authorization
data DwollaTokenResponse = DwollaTokenResponse
  { dtrAccessToken :: DwollaToken
  , dtrTokenType   :: Text
  , dtrExpiresIn   :: Int
  }
  deriving (Eq, Show)
instance FromJSON DwollaTokenResponse where
  parseJSON = withObject "DwollaTokenResponse" $ \o -> do
    dtrAccessToken <- o .: "access_token" <&> encodeUtf8 <&> DwollaToken
    dtrTokenType   <- o .: "token_type"
    dtrExpiresIn   <- o .: "expires_in"
    return $ DwollaTokenResponse { .. }

data ClientErrors
  = RequestError Text
  | CantGenToken
  | MissingInformation Text
  | DwollaPlatformError [DwollaEmbeddedError]
  | DwollaHTTPException HttpExceptionContent
  deriving (Show)

locationInResponse
  :: Either ClientErrors (Response BL.ByteString)
  -> Either ClientErrors ByteString
locationInResponse response =
  response <&> responseHeaders <&> lookup "Location" >>= flatten
 where
  flatten :: Maybe ByteString -> Either ClientErrors ByteString
  flatten loc = case loc of
    Nothing -> Left $ RequestError "Missing location header"
    Just x  -> Right x

getToken :: Either ClientErrors TokenRes -> DwollaTokenResponse
getToken (Right (d, _)) = d
getToken _              = error "not correct"

example :: ByteString -> ByteString -> IO ()
example cid csec = do
  let fromRight (Right x) = x
      fromRight _         = error "Left"
  uid   <- nextRandom
  now   <- getCurrentTime
  trace <- randomTrace
  let us = (defaultUser now (EmailAddress (T.pack (show uid) <> "@example.com")))
        { usrUserID          = UserID uid
        , usrFirstName       = Just "Adam"
        , usrLastName        = Just "J"
        , usrBankAccountName = Just "Checking"
        , usrBankRouting     = Just "222222226"
        , usrBankAcount      = Just "123456789"
        }
  token <-
    createMananger
      >>= createAccessToken "https://api-sandbox.dwolla.com" cid csec
  did <- createCustomer trace us token
  fid <- addBank trace
                 (us { usrDwollaId = Just . decodeUtf8 . fromRight $ did })
                 token
  print did
  print fid

createAccessToken
  :: URLPath
  -> ByteString
  -> ByteString
  -> Manager
  -> IO (Either ClientErrors TokenRes)
createAccessToken base dwollakey secret manager = do
  let
    req :: Either SomeException Request =
      parseRequest (base <> "/token")
        <&> (\r -> r
              { method         = renderStdMethod POST
              , requestBody    = RequestBodyLBS "grant_type=client_credentials"
              , requestHeaders = [ ("User-Agent", "Pay Tgthr/1.0")
                                 , ( "Content-Type"
                                   , "application/x-www-form-urlencoded"
                                   )
                                 ]
              }
            )
        <&> applyBasicAuth dwollakey secret
  let
    excRequest :: Request -> IO (Either HttpException (Response BL.ByteString))
      = try . flip httpLbs manager
  case req of
    Left  exc -> return . Left . RequestError . T.pack $ show exc
    Right r   -> do
      result <- excRequest r
      return $ case result of
        Left (HttpExceptionRequest _ reason) ->
          Left $ DwollaHTTPException reason
        Left  exc      -> Left . RequestError . T.pack $ show exc
        Right response -> if statusCode (responseStatus response) /= 200
          then Left . RequestError . T.pack . show $ responseBody response
          else case eitherDecode $ responseBody response of
            Left  e -> Left . RequestError $ T.pack e
            Right o -> Right (o, (manager, base))

listCustomers
  :: TraceContext
  -> Either ClientErrors TokenRes
  -> IO (Either ClientErrors BL.ByteString)
listCustomers _ (Left e) = return $ Left e
listCustomers trace (Right (DwollaTokenResponse {..}, (manager, base))) =
  (<&> responseBody)
    <$> generateRequest trace
                        dtrAccessToken
                        base
                        manager
                        Nothing
                        basicReqFunc
                        GET
                        "/customers?limit=5&offset=25"
                        empty

idempotencyHash :: ByteString -> Maybe ByteString
idempotencyHash = Just . B16.encode . hash

createCustomer
  :: TraceContext
  -> UserModel
  -> Either ClientErrors TokenRes
  -> IO (Either ClientErrors ByteString)
createCustomer _ _ (Left e) = return $ Left e
createCustomer trace u@UserModel {..} (Right (DwollaTokenResponse {..}, (manager, base)))
  = do
    let reqObj = fromList
          [ "firstName" .= fromMaybe "" usrFirstName
          , "lastName" .= fromMaybe "" usrLastName
          , "email" .= usrEmail
          , "correlationId" .= usrUserID
          ]
    case (usrFirstName, usrLastName) of
      (Nothing, _) -> do
        traceError trace
                   "Error: usrFirstName was empty"
                   (usrUserID, usrRevision, u)
        return . Left $ MissingInformation "usrFirstName"
      (_, Nothing) -> do
        traceError trace
                   "Error: usrLastName was empty "
                   (usrUserID, usrRevision, u)
        return . Left $ MissingInformation "usrLastName"
      (Just _, Just _) -> do
        response <- generateRequest trace
                                    dtrAccessToken
                                    base
                                    manager
                                    (idempotencyHash . fromUID $ usrUserID)
                                    basicReqFunc
                                    POST
                                    "/customers"
                                    reqObj
        return $ locationInResponse response
  where fromUID (UserID uid) = toASCIIBytes uid

addBank
  :: TraceContext
  -> UserModel
  -> Either ClientErrors TokenRes
  -> IO (Either ClientErrors Text)
addBank _ _ (Left e) = return $ Left e
addBank trace u@UserModel {..} (Right (DwollaTokenResponse {..}, (manager, _)))
  = do
    let path = T.unpack (fromMaybe "" usrDwollaId) <> "/funding-sources"
    let bankType = case usrBankType of
          Just (Depository Checking) -> "checking"
          Just (Depository Savings ) -> "savings"
          _                          -> "checking"
    let reqObj = fromList
          [ "routingNumber" .= fromMaybe "" usrBankRouting
          , "accountNumber" .= fromMaybe "" usrBankAcount
          , ("bankAccountType", bankType)
          , "name" .= fromMaybe "" usrBankAccountName
          ]

    let idempotency =
          idempotencyHash (fromRed usrBankAcount <> fromUID usrUserID)

    tracePrint
      trace
      "addBank "
      (usrUserID, usrRevision, path, reqObj, dtrAccessToken, idempotency)

    case (usrDwollaId, usrBankRouting, usrBankAcount) of
      (Nothing, _, _) -> do
        traceError trace
                   "Error: usrDwollaId was empty "
                   (usrUserID, usrRevision, u)
        return . Left $ MissingInformation "usrDwollaId"
      (_, Nothing, _) -> do
        traceError trace
                   "Error: usrBankRouting was empty "
                   (usrUserID, usrRevision, u)
        return . Left $ MissingInformation "usrBankRouting"
      (_, _, Nothing) -> do
        traceError trace
                   "Error: usrBankAcount was empty "
                   (usrUserID, usrRevision, u)
        return . Left $ MissingInformation "usrBankAcount"
      (Just _, Just _, Just _) -> generateRequest trace
                                                  dtrAccessToken
                                                  ""
                                                  manager
                                                  idempotency
                                                  addBankReqFunc
                                                  POST
                                                  path
                                                  reqObj
 where
  fromUID (UserID uid) = toASCIIBytes uid
  fromRed (Just (RedactedText t)) = encodeUtf8 t
  fromRed Nothing =
    error $ "Error: Needs to be a Just" <> show (usrUserID, usrRevision)

removeFundingSource
  :: TraceContext
  -> Text
  -> Either ClientErrors TokenRes
  -> IO (Either ClientErrors (Response BL.ByteString))
removeFundingSource _ _ (Left e) = return $ Left e
removeFundingSource trace fundingId (Right (DwollaTokenResponse {..}, (manager, _)))
  = do
    let path   = T.unpack fundingId
        reqObj = fromList ["removed" .= True]
    generateRequest trace
                    dtrAccessToken
                    ""
                    manager
                    (idempotencyHash $ encodeUtf8 fundingId)
                    basicReqFunc
                    POST
                    path
                    reqObj

createTransfer
  :: TraceContext
  -> Payment
  -> UserModel
  -> Either ClientErrors TokenRes
  -> IO (Either ClientErrors ByteString)
createTransfer _ _ _ (Left e) = return $ Left e
createTransfer trace Payment {..} UserModel {..} (Right (DwollaTokenResponse {..}, (manager, base)))
  = do
    let
      patriotSettlement :: Text
        = "https://api.dwolla.com/funding-sources/00000000-0000-0000-0000-000000000000"
    -- let
    --   dwollaAccountID :: Text
    --     = "https://api.dwolla.com/funding-sources/00000000-0000-0000-0000-000000000000"

    let reqObj = fromList
          [ "_links" .= object
            [ "source" .= case payType of
              DebitFromUser ->
                object ["href" .= fromMaybe "" usrDwollaFundingId]
              CreditToUser -> object ["href" .= patriotSettlement]
            , "destination" .= case payType of
              DebitFromUser -> object ["href" .= patriotSettlement]
              CreditToUser ->
                object ["href" .= fromMaybe "" usrDwollaFundingId]
            ]
          , "amount" .= object
            [ "currency" .= getIsoCode payAmount
            , "value" .= (fromRational (getMonetaryValue payAmount) :: Double)
            ]
          , "metadata" .= object ["paymentId" .= payId, "userId" .= payUser]
          , "correlationId" .= payId
          ]

    tracePrint trace "createTransfer reqObj" (usrUserID, payId, encode reqObj)

    case usrDwollaFundingId of
      Nothing -> do
        traceError trace
                   "Error: usrDwollaFundingId was empty "
                   (usrUserID, usrDwollaId)
        return . Left $ MissingInformation "Funding Id"
      Just _ -> do
        response <- generateRequest trace
                                    dtrAccessToken
                                    base
                                    manager
                                    (idempotencyHash . fromPID $ payId)
                                    basicReqFunc
                                    POST
                                    "/transfers"
                                    reqObj
        return $ locationInResponse response
  where fromPID (PaymentId x) = toASCIIBytes x

cancelTransfer
  :: TraceContext
  -> Payment
  -> Either ClientErrors TokenRes
  -> IO (Either ClientErrors ())
cancelTransfer _     _ (Left e) = return $ Left e
cancelTransfer trace p@Payment { payMethodId = Nothing } _ = do
  traceError trace "No payMethodId" p
  return . Left $ RequestError "No payMethodId"
cancelTransfer trace Payment { payMethodId = Just trxURL, payId } (Right (DwollaTokenResponse {..}, (manager, _)))
  = do
    let reqObj = fromList [("status", "cancelled")]
    tracePrint trace "Cancelling " (payId, trxURL)
    _ <- generateRequest trace
                         dtrAccessToken
                         ""
                         manager
                         (idempotencyHash . fromPID $ payId)
                         basicReqFunc
                         POST
                         (T.unpack trxURL)
                         reqObj
    return $ Right ()
  where fromPID (PaymentId x) = toASCIIBytes x

data DwollaFailureReason = DwollaFailureReason
  { dfrCode        :: PaymentFailureCode
  , dfrDescription :: Text
  , dfrExplanation :: Text
  }
  deriving (Eq, Show)
instance FromJSON DwollaFailureReason where
  parseJSON = withObject "DwollaFailureReason" $ \o -> do
    dfrCodeText :: Text <- o .: "code"
    let dfrCode = case dfrCodeText of
          "R01" -> ACHR01
          "R02" -> ACHR02
          "R03" -> ACHR03
          "R04" -> ACHR04
          "R05" -> ACHR05
          "R07" -> ACHR07
          "R08" -> ACHR08
          "R09" -> ACHR09
          "R10" -> ACHR10
          "R11" -> ACHR11
          "R16" -> ACHR16
          "R20" -> ACHR20
          "R29" -> ACHR29
          x     -> ACHUnknown x
    dfrDescription <- o .: "description"
    dfrExplanation <- o .: "explanation"
    return $ DwollaFailureReason { .. }

getFailureReason
  :: TraceContext
  -> Payment
  -> Either ClientErrors TokenRes
  -> IO (Either ClientErrors DwollaFailureReason)
getFailureReason _     _ (Left e) = return $ Left e
getFailureReason trace p@Payment { payMethodId = Nothing } _ = do
  traceError trace "No payMethodId" p
  return . Left $ RequestError "No payMethodId"
getFailureReason trace Payment { payMethodId = Just trxURL, payId } (Right (DwollaTokenResponse {..}, (manager, _)))
  = do
    tracePrint trace "Getting failure reason " (payId, trxURL)
    response <- generateRequest trace
                                dtrAccessToken
                                ""
                                manager
                                (idempotencyHash . fromPID $ payId)
                                basicReqFunc
                                GET
                                (T.unpack trxURL <> "/failure")
                                empty
    case response of
      Left  e -> return $ Left e
      Right s -> case eitherDecode $ responseBody s of
        Left e ->
          return $ Left $ RequestError $ "can't decode due to: " <> T.pack e
        Right o -> return $ Right o
  where fromPID (PaymentId x) = toASCIIBytes x

createMananger :: IO Manager
createMananger = putStrLn "Creating a new manager" >> newManager
  (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 60000000 }
  )

generateRequest
  :: TraceContext
  -> DwollaToken
  -> String
  -> Manager
  -> Maybe ByteString
  -> (Response BL.ByteString -> Either ClientErrors a)
  -> StdMethod
  -> URLPath
  -> Object
  -> IO (Either ClientErrors a)
generateRequest trace (DwollaToken token) baseURL manager idempotency reqFunc aMethod aPath requestObject
  = do
    let
      req :: Either SomeException Request =
        parseRequest (baseURL <> aPath) <&> \r -> r
          { method         = renderStdMethod aMethod
          , requestBody    = RequestBodyLBS $ encode requestObject
          , requestHeaders =
            [ ("Content-Type" , "application/vnd.dwolla.v1.hal+json")
              , ("Accept"       , "application/vnd.dwolla.v1.hal+json")
              , ("User-Agent"   , "Pay Tgthr/1.0")
              , ("Authorization", "Bearer " <> token)
              ]
              <> fromMaybe [] (idempotency <&> ("Idempotency-Key", ) <&> (: []))
          }
      excRequest :: Request
          -> IO (Either HttpException (Response BL.ByteString))
        = try . flip httpLbs manager
    case req of
      Left exc -> do
        traceError trace
                   "Error: "
                   (exc, baseURL, aMethod, aPath, requestObject, idempotency)
        return . Left . RequestError $ T.pack ("parseRequest: " <> show exc)
      Right r -> do
        result <- excRequest r
        return $ case result of
          Right response -> reqFunc response
          Left (HttpExceptionRequest _ reason) ->
            Left $ DwollaHTTPException reason
          Left exc ->
            Left . RequestError $ T.pack ("excRequest: " <> show exc)

basicReqFunc
  :: Response BL.ByteString -> Either ClientErrors (Response BL.ByteString)
basicReqFunc response = case statusCode (responseStatus response) of
  200 -> Right response
  201 -> Right response
  400 -> case eitherDecode (responseBody response) of
    Left _ -> Left . RequestError . T.pack . show $ responseBody response
    Right DwollaError {..} -> case errorCode of
      DwollaValidationError errors -> Left . DwollaPlatformError $ errors
      _ -> Left . RequestError . T.pack . show $ responseBody response
  _ -> Left . RequestError . T.pack . show $ responseBody response

addBankReqFunc :: Response BL.ByteString -> Either ClientErrors Text
addBankReqFunc response = case statusCode (responseStatus response) of
  200 -> decodeUtf8 <$> locationInResponse (Right response)
  201 -> decodeUtf8 <$> locationInResponse (Right response)
  400 -> case eitherDecode (responseBody response) of
    Left _ -> Left . RequestError . T.pack . show $ responseBody response
    Right DwollaError {..} -> case errorCode of
      DwollaDuplicateResource errorLinks -> Right . linkHref $ errorLinks
      _ -> Left . RequestError . T.pack . show $ responseBody response
  _ -> Left . RequestError . T.pack . show $ responseBody response
