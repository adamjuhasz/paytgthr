{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DeriveGeneric #-}

{- HLINT ignore "Use newtype instead of data" -}

module Shared.Models.Apto.Card
  ( module Shared.Models.Apto.Card
  , CardStatus(..)
  , CardLastFour(..)
  ) where

import           Data.Aeson.Types               ( (.:)
                                                , (.:?)
                                                , (<?>)
                                                , FromJSON(parseJSON)
                                                , JSONPathElement(Key)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Apto.Base        ( AptoCardholderId )
import           Shared.Models.Card             ( AptoCardId
                                                , CardDesign
                                                , CardLastFour(..)
                                                , CardPin
                                                , CardStatus(..)
                                                )

data AptoCardCreateRequest = AptoCardCreateRequest
  { acdrDesign :: CardDesign
  }
  deriving (Eq, Show)
instance ToJSON AptoCardCreateRequest where
  toJSON AptoCardCreateRequest {..} =
    object ["design_key" .= acdrDesign, ("program_id", "Paytogether")]

data AptoCard = AptoCard
  { acdxId           :: AptoCardId
  , acdxProgram      :: Text
  , acdxDesign       :: CardDesign
  , acdxLastFour     :: CardLastFour
  , acdxStatus       :: CardStatus
  , acdxActivatedAt  :: Maybe UTCTime
  , acdxCreatedAt    :: UTCTime
  , acdxCardholderId :: AptoCardholderId
  , acdxDDANumber    :: Maybe Text
  , acdxABARouting   :: Maybe Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON AptoCard where
  parseJSON = withObject "AptoCard" $ \o -> do
    cd               <- o .: "card"

    acdxId           <- cd .: "id" <?> Key "card"
    acdxProgram      <- cd .: "program_id" <?> Key "card"
    acdxDesign       <- cd .: "design_key" <?> Key "card"
    acdxLastFour     <- cd .: "last_four" <?> Key "card"
    acdxStatus       <- cd .: "status" <?> Key "card"
    acdxActivatedAt  <- cd .:? "activated_at" <?> Key "card"
    acdxCreatedAt    <- cd .: "created_at" <?> Key "card"
    acdxCardholderId <- cd .: "cardholder_id" <?> Key "card"
    acdxDDANumber    <- cd .: "dda_number" <?> Key "card"
    acdxABARouting   <- cd .: "aba_routing_number" <?> Key "card"
    return $ AptoCard { .. }
instance ToJSON AptoCard where
  toJSON AptoCard {..} = object
    [ "card" .= object
        [ "id" .= acdxId
        , "program_id" .= acdxProgram
        , "design_key" .= acdxDesign
        , "last_four" .= acdxLastFour
        , "status" .= acdxStatus
        , "activated_at" .= acdxActivatedAt
        , "created_at" .= acdxCreatedAt
        , "cardholder_id" .= acdxCardholderId
        , "dda_number" .= acdxDDANumber
        , "aba_routing_number" .= acdxABARouting
        ]
    ]

data AptoCardResponse = AptoCardResponse
  { acrxCards :: [AptoCard]
  }
  deriving (Eq, Show)
instance FromJSON AptoCardResponse where
  parseJSON = withObject "AptoCardResponse" $ \o -> do
    ch        <- o .: "cardholder"
    acrxCards <- ch .: "cards"

    return $ AptoCardResponse { .. }

data AptoCardChangePinRequest = AptoCardChangePinRequest
  { acprCardId :: AptoCardId
  , acprPin    :: CardPin
  }
  deriving (Eq, Show)
instance ToJSON AptoCardChangePinRequest where
  toJSON AptoCardChangePinRequest {..} =
    object ["card" .= acprCardId, "new_pin" .= acprPin]

data AptoCardStatusAction
  = Activate
  | Deactivate
  | Open
  | Reissue
  | Close
  deriving (Eq, Show)
instance ToJSON AptoCardStatusAction where
  toJSON Activate   = "ACTIVATE"
  toJSON Deactivate = "DEACTIVATE"
  toJSON Open       = "OPEN"
  toJSON Reissue    = "REISSUE"
  toJSON Close      = "CLOSE"

data AptoCardChangeStatusRequest = AptoCardChangeStatusRequest
  { acsrCardId :: AptoCardId
  , acsrAction :: AptoCardStatusAction
  }
  deriving (Eq, Show)
instance ToJSON AptoCardChangeStatusRequest where
  toJSON AptoCardChangeStatusRequest {..} =
    object ["card" .= acsrCardId, "action" .= acsrAction]

data AptoCardOderRequest = AptoCardOderRequest
  { acorCardId       :: AptoCardId
  , acorCardholderId :: AptoCardholderId
  }
  deriving (Eq, Show)
instance ToJSON AptoCardOderRequest where
  toJSON AptoCardOderRequest {..} =
    object ["card" .= acorCardId, "cardholder" .= acorCardholderId]

