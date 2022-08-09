{-# LANGUAGE RecordWildCards #-}

module Shared.Console where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( KeyValue((.=))
                                                , encode
                                                , object
                                                )
import           Data.ByteString.Lazy.Char8    as C8
                                                ( putStrLn )
import           Shared.WebAPI.General.API      ( TraceContext(..) )
import           Text.Printf                    ( printf )

tracePrint :: (Show a, MonadIO m) => TraceContext -> String -> a -> m ()
tracePrint = traceSeverity DEFAULT ""

traceError :: (Show a, MonadIO m) => TraceContext -> String -> a -> m ()
traceError = traceSeverity ERROR "Error: "

data Severity
  = DEFAULT
  | DEBUG
  | INFO
  | NOTICE
  | WARNING
  | ERROR
  | CRITICAL
  | ALERT
  | EMERGENCY
  deriving Show

traceSeverity
  :: (Show a, MonadIO m)
  => Severity
  -> String
  -> TraceContext
  -> String
  -> a
  -> m ()
traceSeverity severity preStr TraceContext {..} str val = do
  let obj = object
        [ "logging.googleapis.com/trace"
          .= ("projects/infra-tgthr/traces/" <> traceId)
        , "logging.googleapis.com/spanId" .= (printf "%016x" spanId :: String)
        , "logging.googleapis.com/trace_sampled" .= False
        , "message" .= (preStr <> str <> " " <> show val)
        , "severity" .= show severity
        ]
  liftIO $ C8.putStrLn $ encode obj
