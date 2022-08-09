module Shared.Track.HasTracing where

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime
                                                , diffUTCTime
                                                )
import           Network.HTTP.Client            ( Manager )
import           Shared.Console                 ( tracePrint )
import           Shared.Track.Models.Stackdriver
                                               as StackDriver
                                                ( CreateSpan(..) )
import           Shared.Track.Stackdriver       ( sendToStackDriver )
import           Shared.WebAPI.General.API     as API
                                                ( TraceContext(..) )
import           Text.Printf                    ( printf )
import           Web.JWT                        ( Signer )

class Monad m => HasTracing m where
  traceSpan      :: Text -> TraceContext -> TraceContext -> m a -> m a
  traceChildSpan :: Text -> TraceContext -> m a -> m a

trackSpan
  :: Manager
  -> (Text, Signer, Text)
  -> Text
  -> TraceContext
  -> TraceContext
  -> UTCTime
  -> UTCTime
  -> IO ()
trackSpan manager jwt name parentTrace thisTrace sTime eTime = do
  let intToText = T.pack . printf "%016x"

  tracePrint thisTrace
             "trackSpan"
             (name, parentTrace, thisTrace, diffUTCTime eTime sTime)

  let newSpan = CreateSpan
        { StackDriver.traceId = API.traceId thisTrace
        , StackDriver.spanId  = intToText $ API.spanId thisTrace
        , parentSpanId        = Just . intToText $ API.spanId parentTrace
        , displayName         = name
        , startTime           = sTime
        , endTime             = eTime
        , attributes          = []
        , status              = Nothing
        , kind                = Nothing
        , linkParent          = Nothing
        }

  sendToStackDriver manager jwt newSpan
    `catch` (\(e :: SomeException) ->
              tracePrint parentTrace "Error: sendToStackDriver" e
            )
