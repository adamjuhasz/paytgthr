{-# OPTIONS_GHC -Wno-deprecations #-}
{- HLINT ignore "Use lambda-case" -}

module Shared.Web.Utils where

import           Control.Exception              ( Exception(fromException)
                                                , SomeException
                                                )
import           Control.Monad                  ( when )
import qualified Data.ByteString.Char8         as S8
import qualified Data.ByteString.Lazy          as BL
import           Data.Function                  ( (&) )
import           Network.HTTP.Types             ( badRequest400
                                                , hContentType
                                                , internalServerError500
                                                , status413
                                                , status431
                                                )
import           Network.HTTP2                  ( ErrorCodeId(UnknownErrorCode)
                                                , HTTP2Error(ConnectionError)
                                                )
import           Network.Wai                    ( Request
                                                , Response
                                                , responseLBS
                                                )
import           Network.Wai.Handler.Warp       ( InvalidRequest
                                                , Settings
                                                , defaultSettings
                                                , defaultShouldDisplayException
                                                , setOnException
                                                , setOnExceptionResponse
                                                )
import           Shared.Amqp                    ( MessageBody
                                                , SentMessage
                                                , TgthrMessage
                                                )

type Publisher = Maybe TgthrMessage -> MessageBody -> IO SentMessage

warpDefaultSettings :: Network.Wai.Handler.Warp.Settings
warpDefaultSettings =
  Network.Wai.Handler.Warp.defaultSettings
    & setOnException warpExcptionHandler
    & setOnExceptionResponse warpExceptionResponse

-- cribbed from defaultOnException
warpExcptionHandler :: Maybe Request -> SomeException -> IO ()
warpExcptionHandler Nothing e =
  when (defaultShouldDisplayException e) (print e)
warpExcptionHandler (Just r) e = do
  putStr "Warp error <> Excption: "
  putStr $ show e
  putStr " <> Request: "
  print r

-- cribbed from defaultOnExceptionResponse
warpExceptionResponse :: SomeException -> Response
warpExceptionResponse e
  | Just (_ :: InvalidRequest) <- fromException e
  = responseLBS badRequest400
                [(hContentType, "text/plain; charset=utf-8")]
                "Bad Request"
  | Just (ConnectionError (UnknownErrorCode 413) t) <- fromException e
  = responseLBS status413
                [(hContentType, "text/plain; charset=utf-8")]
                (BL.fromStrict t)
  | Just (ConnectionError (UnknownErrorCode 431) t) <- fromException e
  = responseLBS status431
                [(hContentType, "text/plain; charset=utf-8")]
                (BL.fromStrict t)
  | otherwise
  = responseLBS internalServerError500
                [(hContentType, "text/plain; charset=utf-8")]
                (BL.fromStrict . S8.pack $ show e)

