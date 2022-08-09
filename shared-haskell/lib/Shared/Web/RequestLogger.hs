{-# LANGUAGE QuasiQuotes #-}

module Shared.Web.RequestLogger
  ( formatAsJSONWithHeaders
  , headerToSpan
  , headerToTrace
  , replaceUUIDWithmarker
  ) where

import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value
                                                , encode
                                                , object
                                                )
import           Data.ByteString                ( ByteString )
-- import qualified Data.ByteString.Builder       as BB
--                                                 ( toLazyByteString )
-- import           Data.ByteString.Lazy           ( toStrict )
import           Data.CaseInsensitive           ( original )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8
                                                , decodeUtf8With
                                                )
import           Data.Text.Encoding.Error       ( lenientDecode )
import           Network.HTTP.Types             ( Header
                                                , Status(statusCode)
                                                )
import           Network.Wai                    ( Request
                                                  ( rawPathInfo
                                                  , requestHeaders
                                                  , requestMethod
                                                  )
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( OutputFormatterWithDetailsAndHeaders
                                                )
import           Network.Wai.Middleware.RequestLogger.JSON
                                                ( requestToJSON )
import           System.Log.FastLogger          ( toLogStr )
import           Text.Printf                    ( printf )
import           Text.RawString.QQ              ( r )
import           Text.Read                      ( readMaybe )
import           Text.Regex.TDFA                ( (=~) )
import           Text.Regex.TDFA.Text           ( )

uuidRegex :: String
uuidRegex =
  [r|[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}|]

replaceUUIDWithmarker :: Text -> Text
replaceUUIDWithmarker incom =
  let match = incom =~ uuidRegex
  in  if match == ""
        then incom
        else replaceUUIDWithmarker $ T.replace match ":uuid" incom

formatAsJSONWithHeaders :: OutputFormatterWithDetailsAndHeaders
formatAsJSONWithHeaders date req status resSize duration reqBody _res resHeaders
  = let
      path           = decodeUtf8With lenientDecode (rawPathInfo req)
      pathWithMarker = replaceUUIDWithmarker path
    in
      toLogStr
          (encode $ object
            [ "request" .= requestToJSON req reqBody (Just duration)
            , "httpRequest" .= object
              [ "requestMethod"
                .= decodeUtf8With lenientDecode (requestMethod req)
              , "requestUrl" .= decodeUtf8With lenientDecode (rawPathInfo req)
              , "status" .= statusCode status
              , "latency"
                .= ((printf "%.5fs" . rationalToDouble $ toRational duration) :: String
                   )
              , "userAgent" .= (decodeUtf8With lenientDecode <$> ua)
              , "remoteIp" .= remoteIp
              , "referer" .= (decodeUtf8With lenientDecode <$> ref)
              ]
            , "response" .= object
              [ "status" .= statusCode status
              , "size" .= resSize
              , "headers" .= responseHeadersToJSON resHeaders
            -- , "body"
            --   .= ( decodeUtf8With lenientDecode
            --      . toStrict
            --      . BB.toLazyByteString
            --      $ res
            --      )
              , "duration" .= duration
              ]
            , "route" .= pathWithMarker
            , "time" .= decodeUtf8With lenientDecode date
            , "logging.googleapis.com/trace" .= case traceHeader of
              Just h  -> Just $ "projects/infra-tgthr/traces/" <> h
              Nothing -> Nothing
            , "logging.googleapis.com/spanId" .= traceSpan
            , "logging.googleapis.com/trace_sampled" .= False
            , ("severity", "DEBUG")
            ]
          )
        <> "\n"
 where
  rationalToDouble :: Rational -> Double
  rationalToDouble = fromRational
  reqHeaders       = requestHeaders req
  traceHeader      = lookup "X-Cloud-Trace-Context" reqHeaders >>= headerToTrace -- ex: "0a369d265d974a278032388a2bd48f2c/693637;o=TRACE_TRUE"
  traceSpan        = lookup "X-Cloud-Trace-Context" reqHeaders >>= headerToSpan
  ua               = lookup "user-agent" reqHeaders
  remoteIp =
    lookup "X-Forwarded-For" reqHeaders
      >>= (safeHead . T.splitOn ", " . decodeUtf8With lenientDecode)
  ref = lookup "referer" reqHeaders

safeLast :: [a] -> Maybe a
safeLast []       = Nothing
safeLast (x : xs) = Just (foldl (\_ a -> a) x xs)

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

headerToTrace :: ByteString -> Maybe Text
headerToTrace bs = case T.splitOn "/" . decodeUtf8 $ bs of
  []      -> Nothing
  [   _ ] -> Nothing
  txt : _ -> Just txt

headerToSpan :: ByteString -> Maybe Text
headerToSpan bs =
  (safeLast . T.splitOn "/" $ decodeUtf8 bs)
    >>= (safeHead . T.splitOn ";")
    >>= ((readMaybe :: String -> Maybe Int) . T.unpack)
    >>= (Just . T.pack . printf "%016x")

responseHeadersToJSON :: [Header] -> Value
responseHeadersToJSON = toJSON . map hToJ where
  -- Redact cookies
  hToJ ("Set-Cookie", _) = toJSON ("Set-Cookie" :: Text, "-RDCT-" :: Text)
  hToJ hd                = headerToJSON hd

headerToJSON :: Header -> Value
headerToJSON (headerName, header) = toJSON
  ( decodeUtf8With lenientDecode . original $ headerName
  , decodeUtf8With lenientDecode header
  )
