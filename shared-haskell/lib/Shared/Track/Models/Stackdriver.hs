{-# LANGUAGE RecordWildCards #-}
module Shared.Track.Models.Stackdriver where

import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value(Null)
                                                , object
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )

data CreateSpan = CreateSpan
  { traceId      :: Text
  , spanId       :: Text
  , parentSpanId :: Maybe Text
  , displayName  :: Text
  , startTime    :: UTCTime
  , endTime      :: UTCTime
  , attributes   :: [(Text, Maybe Text)]
  , status       :: Maybe Int
  , kind         :: Maybe Text
  , linkParent   :: Maybe (Text, Text)
  }
  deriving (Eq, Show)

instance ToJSON CreateSpan where
  toJSON CreateSpan {..} = object
    [ "name"
      .= ("projects/infra-tgthr/traces/" <> traceId <> "/spans/" <> spanId)
    , "spanId" .= spanId
    , "parentSpanId" .= parentSpanId
    , "displayName"
      .= object ["value" .= displayName, "truncatedByteCount" .= (0 :: Int)]
    , "startTime" .= startTime
    , "endTime" .= endTime
    , "attributes" .= object ["attributeMap" .= toAttributeMap attributes]
    , "stackTrace" .= Null
    , "status" .= case status of
      Just s  -> object ["code" .= s]
      Nothing -> Null
    , "spanKind" .= kind
    , "sameProcessAsParentSpan" .= False
    , "links" .= case linkParent of
      Nothing              -> Null
      Just (aTrace, aSpan) -> object
        [ "link"
            .= [ object
                   [ "traceId" .= aTrace
                   , "spanId" .= aSpan
                   , "type" .= ("PARENT_LINKED_SPAN" :: Text)
                   ]
               ]
        ]
    ]

toAttributeMap :: [(Text, Maybe Text)] -> Value
toAttributeMap array = object $ foldr attrConv [] array
 where
  attrConv (_, Nothing) acc = acc
  attrConv (k, Just v) acc =
    (k .= object
        [ "stringValue"
            .= object ["value" .= v, "truncatedByteCount" .= (0 :: Int)]
        ]
      )
      : acc
