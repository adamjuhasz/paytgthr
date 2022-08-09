{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData, RecordWildCards #-}

module APIApto.Apto.Client where

import           Network.HTTP.Client            ( Manager
                                                , HttpExceptionContent
                                                , responseTimeoutMicro
                                                , httpLbs
                                                , newManager
                                                , applyBasicAuth
                                                , parseRequest
                                                , HttpException(..)
                                                , ManagerSettings(..)
                                                , Request(..)
                                                , RequestBody(RequestBodyLBS)
                                                , Response(..)
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Types.Status      ( Status
                                                , statusCode
                                                )
import           Network.HTTP.Types.Method      ( StdMethod
                                                , renderStdMethod
                                                )
import           Data.Pool                      ( Pool
                                                , withResource
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC8
import qualified Data.ByteString.Lazy          as BL
import           Data.Aeson.Types               ( Object )
import           Control.Exception              ( try )
import           Data.Aeson                     ( eitherDecode
                                                , encode
                                                )
import           APIApto.Model.AptoAPIError     ( AptoAPIError )

type URLPath = String

data AptoSecrets = AptoSecrets
  { baseURL :: URLPath
  , username :: B.ByteString
  , password :: B.ByteString
  } deriving (Eq)

data RequesterError
  = StatusError Status BL.ByteString
  | APIError Status AptoAPIError
  | ExceptionError HttpExceptionContent
  | UnknownError String
  | DecodeError String
  deriving (Show)

type Requester
  = StdMethod -> URLPath -> Object -> IO (Either RequesterError BL.ByteString)
type RequesterWithID = String -> Requester
type OpaqueManager = Pool (Manager, AptoSecrets)

createMananger :: b -> IO (Manager, b)
createMananger secrets = do
  Prelude.putStrLn "Creating a new manager"
  manager <- newManager $ tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro 60000000
    }
  return (manager, secrets)

requestWithPool :: OpaqueManager -> RequesterWithID
requestWithPool pool mid aMethod aPath reqObject =
  withResource pool $ \(manager, secrets) ->
    generateRequest secrets manager mid aMethod aPath reqObject

generateRequest :: AptoSecrets -> Manager -> RequesterWithID
generateRequest AptoSecrets {..} manager mid aMethod aPath requestObject = do
  let
    req :: IO (Either HttpException Request) =
      try $ parseRequest (baseURL <> aPath) >>= \r -> return $ r
        { method          = renderStdMethod aMethod
        , requestBody     = RequestBodyLBS $ encode requestObject
        , requestHeaders  = [ ("Content-Type"    , "application/json")
                            , ("Accept"          , "application/json")
                            , ("User-Agent"      , "Pay Tgthr/1.3")
                            , ("x-paytgthr-trace", BC8.pack mid)
                            ]
        , responseTimeout = responseTimeoutMicro 70000000
        }
    excRequest :: Request -> IO (Either HttpException (Response BL.ByteString))
      = try . flip httpLbs manager . applyBasicAuth username password
  parsed <- req
  case parsed of
    Left exc -> do
      putStr "Error with generateRequest req: "
        >> print (mid, aMethod, aPath, exc)
      returnException exc
    Right r -> do
      putStr "Starting request " >> print (mid, aMethod, aPath)
      result <- excRequest r
      putStr "Completed request " >> print (mid, aMethod, aPath)
      case result of
        Left exc -> returnException exc
        Right response ->
          let status   = responseStatus response
              httpcode = statusCode status
              body     = responseBody response
          in  case (httpcode, eitherDecode body) of
                (200, _) -> return $ Right body
                (_, Right apiError) ->
                  printErrorStatus response
                    >> (return . Left $ APIError status apiError)
                (_, Left _) ->
                  printErrorStatus response
                    >> (return . Left $ StatusError status body)
 where
  returnException :: HttpException -> IO (Either RequesterError BL.ByteString)
  returnException exc = case exc of
    InvalidUrlException url reason ->
      return . Left . UnknownError $ url <> " " <> reason
    HttpExceptionRequest _ content -> return . Left . ExceptionError $ content
  printErrorStatus :: Response BL.ByteString -> IO ()
  printErrorStatus response =
    putStr "Error with generateRequest response: " >> print
      ( mid
      , aMethod
      , aPath
      , responseStatus response
      , responseHeaders response
      , responseBody response
      )
