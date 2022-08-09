{-# LANGUAGE RecordWildCards #-}

module Shared.Track.Stackdriver
  ( getStackdriverSecrets
  , stackDriverMiddleware
  , sendToStackDriver
  ) where

import           Control.Concurrent.Async       ( async )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Data.Aeson                     ( KeyValue((.=))
                                                , Object
                                                , encode
                                                , object
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import qualified Data.ByteString.Char8         as BC
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text.Encoding            as TE
import           Data.Text.Encoding.Error       ( lenientDecode )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Network.HTTP.Client            ( Manager
                                                , ManagerSettings
                                                  ( managerResponseTimeout
                                                  )
                                                , Request
                                                  ( method
                                                  , requestBody
                                                  , requestHeaders
                                                  )
                                                , RequestBody(RequestBodyLBS)
                                                , Response
                                                  ( responseBody
                                                  , responseStatus
                                                  )
                                                , httpLbs
                                                , newManager
                                                , parseRequest
                                                , responseTimeoutMicro
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Types             ( RequestHeaders
                                                , Status(statusCode)
                                                , StdMethod(POST)
                                                , renderStdMethod
                                                )
import qualified Network.Wai                   as Wai
import           Shared.Track.Models.Stackdriver
                                                ( CreateSpan(..) )
import           Shared.Utils                   ( fromRight )
import           Shared.Vault                   ( extractSecret )
import           Shared.Web.RequestLogger       ( headerToSpan
                                                , headerToTrace
                                                , replaceUUIDWithmarker
                                                )
import           Web.JWT                        ( Algorithm(RS256)
                                                , JOSEHeader(alg, kid, typ)
                                                , JWTClaimsSet
                                                  ( aud
                                                  , exp
                                                  , iat
                                                  , iss
                                                  , sub
                                                  )
                                                , Signer(RSAPrivateKey)
                                                , StringOrURI
                                                , encodeSigned
                                                , numericDate
                                                , readRsaSecret
                                                , stringOrURI
                                                )

replace :: String -> String
replace ('\\' : 'n' : xs) = '\n' : replace xs
replace (x          : xs) = x : replace xs
replace ""                = ""

getStackdriverSecrets
  :: (String -> IO Object) -> IO (Manager, (Text, Signer, Text))
getStackdriverSecrets secretGetter = do
  tlsManager    <- createMananger
  googleSecrets <- secretGetter "tracking"
  let googKey =
        RSAPrivateKey
          . fromJust
          . readRsaSecret
          . BC.pack
          . replace
          $ extractSecret googleSecrets "gsa_private_key"
  let googEmail = pack $ extractSecret googleSecrets "gsa_client_email"
  let googKeyId = pack $ extractSecret googleSecrets "gsa_private_key_id"
  return (tlsManager, (googEmail, googKey, googKeyId))

stackDriverMiddleware :: (Manager, (Text, Signer, Text)) -> Wai.Middleware
stackDriverMiddleware (tlsManager, jwtInfo) app req respond = do
  start <- getCurrentTime
  app req $ \res -> do
    end <- getCurrentTime
    sendRequest tlsManager jwtInfo req res start end
    respond res

sendRequest
  :: Manager
  -> (Text, Signer, Text)
  -> Wai.Request
  -> Wai.Response
  -> UTCTime
  -> UTCTime
  -> IO ()
sendRequest tlsManager jwtInfo request response startTime endTime = do
  let reqHeaders   = Wai.requestHeaders request
  let cloudContext = lookup "X-Cloud-Trace-Context" reqHeaders
  let traceIdM     = cloudContext >>= headerToTrace
  let spanIdM      = cloudContext >>= headerToSpan
  let parentSpanId = Nothing
  let path = replaceUUIDWithmarker
        $ TE.decodeUtf8With lenientDecode (Wai.rawPathInfo request)
  let method      = TE.decodeUtf8 $ Wai.requestMethod request
  let displayName = method <> " " <> path
  let status      = Just $ statusCode $ Wai.responseStatus response
  let
    attributes =
      mapHeaders reqHeaders
        <> [ ( "/http/status_code"
             , Just . pack $ show $ statusCode $ Wai.responseStatus response
             )
           , ("type"        , Just "event")
           , ("/http/method", Just method)
           , ("/http/path"  , Just path)
           , ( "/http/user_agent"
             , TE.decodeUtf8 <$> Wai.requestHeaderUserAgent request
             )
           , ( "/http/client_city"
             , TE.decodeUtf8 <$> lookup "CF-IPCity" reqHeaders
             )
           , ( "/http/client_region"
             , TE.decodeUtf8 <$> lookup "CF-IPRegion" reqHeaders
             )
           , ( "/paytgthr.com/http/pathraw"
             , Just $ TE.decodeUtf8With lenientDecode $ Wai.rawPathInfo request
             )
           ]
  let kind       = Nothing
  let linkParent = Nothing

  case (traceIdM, spanIdM) of
    (Just traceId, Just spanId) ->
      void $ async $ sendToStackDriver tlsManager jwtInfo CreateSpan { .. }
    (_, _) -> return ()

mapHeaders :: RequestHeaders -> [(Text, Maybe Text)]
mapHeaders reqHeaders =
  let
    decoder Nothing  = Nothing
    decoder (Just b) = Just $ TE.decodeUtf8 b
  in
    second decoder
      <$> [ ("/http/request/size", lookup "content-length" reqHeaders)
          , ("/http/host"                    , lookup "host" reqHeaders)
          , ("/http/client_country"          , lookup "cf-ipcountry" reqHeaders)
          , ("/http/client_protocol", lookup "x-forwarded-proto" reqHeaders)
          , ("/http/request/size", lookup "content-length" reqHeaders)
          , ("paytgthr.com/http/origin"      , lookup "origin" reqHeaders)
          , ("paytgthr.com/http/referer"     , lookup "referer" reqHeaders)
          , ("paytgthr.com/client/ip", lookup "cf-connecting-ip" reqHeaders)
          , ("paytgthr.com/http/forwarded", lookup "x-forwarded-for" reqHeaders)
          , ("paytgthr.com/http/content-type", lookup "content-type" reqHeaders)
          ]

sendToStackDriver :: Manager -> (Text, Signer, Text) -> CreateSpan -> IO ()
sendToStackDriver manager (email, key, keyId) aSpan = do
  jwtClaims <- createClaims email
  let spanPack  = object ["spans" .= [aSpan]]
      jwtHeader = createHeader keyId
      jwt       = encodeSigned key jwtHeader jwtClaims
  -- print $ encode spanPack

  let
    req' :: Either SomeException Request =
      parseRequest
          "https://cloudtrace.googleapis.com/v2/projects/infra-tgthr/traces:batchWrite"
        <&> \req -> req
              { method         = renderStdMethod POST
              , requestBody    = RequestBodyLBS $ encode spanPack
              , requestHeaders = [ ("Content-Type", "application/json")
                                 , ("Accept"      , "application/json")
                                 , ("User-Agent"  , "Pay Tgthr/1.0")
                                 , ( "Authorization"
                                   , TE.encodeUtf8 $ "Bearer " <> jwt
                                   )
                                 ]
              }
  res <-
    (Just <$> httpLbs (fromRight req') manager)
      `catch` (\(e :: SomeException) -> do
                putStr "Warning: sendToStackDriver failed " >> print e
                return Nothing
              )
  when ((statusCode . responseStatus <$> res) /= Just 200) $ do
    putStr "Warning: Stackdriver failed "
    print (responseStatus <$> res, responseBody <$> res)

createMananger :: IO Manager
createMananger = newManager $ tlsManagerSettings
  { managerResponseTimeout = responseTimeoutMicro 60000000
  }

createHeader :: Text -> JOSEHeader
createHeader keyId =
  mempty { typ = Just "JWT", alg = Just RS256, kid = Just keyId }

createClaims :: Text -> IO JWTClaimsSet
createClaims email = do
  epoch <- getPOSIXTime
  let audience :: Maybe (Either StringOrURI [StringOrURI])
      audience = Just . Left . fromJust $ stringOrURI
        "https://cloudtrace.googleapis.com/"
  return $ mempty { iss         = stringOrURI email
                  , sub         = stringOrURI email
                  , aud         = audience
                  , Web.JWT.exp = numericDate $ epoch + 3600
                  , iat         = numericDate epoch
                  }
