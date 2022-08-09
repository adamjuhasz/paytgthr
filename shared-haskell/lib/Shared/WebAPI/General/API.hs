{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}

module Shared.WebAPI.General.API
  ( traceToMID
  , uuidToTrace
  , randomTrace
  , incrementTrace
  , midToTrace
  , parseTrace
  , TraceContext(..)
  , TraceHeaders
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.UUID                      ( UUID
                                                , fromText
                                                , toText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Servant                        ( FromHttpApiData(parseUrlPiece)
                                                , Header'
                                                , Required
                                                , Strict
                                                , ToHttpApiData(toUrlPiece)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           System.Random                  ( randomRIO)
import           Text.Read                      ( readEither )

traceToMID :: TraceContext -> MessageID
traceToMID t@TraceContext {..} =
  let
    (firstSection, t'   ) = T.splitAt 8 traceId
    (second      , t''  ) = T.splitAt 4 t'
    (third       , t''' ) = T.splitAt 4 t''
    (fourth      , t'''') = T.splitAt 4 t'''
    assembedUUID =
      firstSection
        <> "-"
        <> second
        <> "-"
        <> third
        <> "-"
        <> fourth
        <> "-"
        <> t''''
    parsedUUID = fromText assembedUUID
  in
    case parsedUUID of
      Nothing ->
        error $ "Could not parse TraceContext " <> show (t, assembedUUID)
      Just u -> MessageID u

midToTrace :: MonadIO m => MessageID -> m TraceContext
midToTrace (MessageID uuid) = incrementTrace $ uuidToTrace uuid

uuidToTrace :: UUID -> TraceContext
uuidToTrace uuid =
  let traceId = T.replace "-" "" . toText $ uuid
      spanId  = 1
  in  TraceContext { .. }

randomTrace :: (MonadIO m) => m TraceContext
randomTrace = do
  randomUUID <- liftIO nextRandom
  return $ uuidToTrace randomUUID

incrementTrace :: (MonadIO m) => TraceContext -> m TraceContext
incrementTrace trace = do
  newSpanId <- liftIO $ randomRIO (2, 999999)
  return $ trace { spanId = newSpanId }

parseTrace :: Text -> Either Text TraceContext
parseTrace t =
  let (traceId   , restOf) = T.breakOn "/" t
      (spanIdText, _     ) = T.breakOn ";" . T.drop 1 $ restOf
      spanIdEither         = readEither $ T.unpack spanIdText
  in  case spanIdEither of
        Left  s      -> Left $ T.pack s
        Right spanId -> Right $ TraceContext { .. }

data TraceContext = TraceContext
  { traceId :: Text
  , spanId  :: Int
  }
  deriving (Eq, Show)
instance FromHttpApiData TraceContext where
  parseUrlPiece = parseTrace
instance ToHttpApiData TraceContext where
  toUrlPiece TraceContext {..} =
    traceId <> "/" <> T.pack (show spanId) <> ";o=TRACE_TRUE"

type TraceHeaders
  = Header' '[Required , Strict] "X-Cloud-Trace-Context" TraceContext
