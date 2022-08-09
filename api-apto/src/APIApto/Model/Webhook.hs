{-# LANGUAGE DeriveGeneric, DeriveAnyClass  #-}
{-# LANGUAGE StrictData, RecordWildCards #-}

module APIApto.Model.Webhook
  ( module APIApto.Model.Webhook
  , AptoTransaction(..)
  ) where

import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (<?>)
                                                , FromJSON(..)
                                                , Value(..)
                                                , withObject
                                                , withText
                                                )
import           Data.Aeson.Types               ( JSONPathElement(Key)
                                                , Parser
                                                )
import           Data.HashMap.Strict            ( keys )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Apto.Base        ( AptoCardholderId )
import           Shared.Models.Apto.Card        ( CardLastFour
                                                , CardStatus
                                                )
import           Shared.Models.Apto.Cardholder  ( AptoCardholderResponse(..)
                                                , KYCEvent(..)
                                                )
import           Shared.Models.Apto.Transaction ( AptoTransaction(..) )
import           Shared.Models.Card             ( AptoCardId
                                                , CardDesign
                                                )

-- | Parsing v2 of the Apto Webhooks
--  
--  Format is {"event": { ..., data: { ..prev object shape.. } }}
parseEvent :: Value -> Parser AptoWebhook
parseEvent = withObject "AptoWebhook" $ \event -> do
  -- inside event
  _eventId :: Text       <- event .: "id" <?> Key "event"
  _created_at :: UTCTime <- event .: "created_at" <?> Key "event"
  eventType :: Text      <- event .: "event_type" <?> Key "event"
  dataType :: Text       <- event .: "data_type" <?> Key "event"
  dataVal :: Value       <- event .: "data" <?> Key "event"

  -- repackage to match v1 style
  case dataType of
    "cardholder" -> KYCNot <$> do
      cardholderEvent :: KYCEvent      <- parseJSON (String eventType)
      parsed :: AptoCardholderResponse <- parseJSON dataVal
      return parsed { accxEvent = Just cardholderEvent }
    "card" -> CardNot <$> do
      cardEvent <- parseJSON (String eventType)
      parseCardUpdateFromJSON cardEvent dataVal
    "transaction" -> TransactionNot <$> parseJSON dataVal
    t             -> fail $ "data_type not recognized: " <> show t

data AptoWebhook
  = TransactionNot AptoTransaction
  | KYCNot AptoCardholderResponse
  | CardNot CardUpdate
  | SettlementNot SettlementNotify
  deriving (Eq, Show)

instance FromJSON AptoWebhook where
  parseJSON = withObject "AptoWebhook" $ \o -> do
    trx :: Maybe Value        <- o .:? "transaction"
    kyc :: Maybe Value        <- o .:? "cardholder"
    card :: Maybe Value       <- o .:? "card"
    settlement :: Maybe Value <- o .:? "settlement"
    event :: Maybe Value      <- o .:? "event"
    let is = isJust
    -- not using assum [] because we want to generate better error messages
    case (is trx, is kyc, is card, is settlement, event) of
      (True, _, _, _, _) -> TransactionNot <$> parseJSON (Object o)
      (_, True, _, _, _) -> KYCNot <$> parseJSON (Object o)
      (_, _, True, _, _) -> CardNot <$> parseJSON (Object o)
      (_, _, _, True, _) -> SettlementNot <$> parseJSON (Object o)
      (_, _, _, _, Just v) -> parseEvent v
      (_, _, _, _, _) -> fail $ "Key not recognized, I see: " <> show (keys o)

data CardUpdateEvent
  = PinUpdated
  | StatusUpdate
  | CardShipped
  deriving (Eq, Show)
instance FromJSON CardUpdateEvent where
  parseJSON = withText "CardUpdateEvent" $ \t -> case t of
    "pin_update"    -> return PinUpdated
    "status_update" -> return StatusUpdate
    "card_a"     -> return CardShipped
    _ -> fail $ "CardUpdateEvent doesn't recognize: \"" <> T.unpack t <> "\""

data CardUpdate = CardUpdate
  { crdId           :: AptoCardId
  , crdCardholderId :: AptoCardholderId
  , crdDesign       :: CardDesign
  , crdStatus       :: CardStatus
  , crdLastFour     :: Maybe CardLastFour
  , crdCreated      :: Maybe UTCTime
  , crdActivated    :: Maybe UTCTime
  , crdProgram      :: Text
  , crdEvent        :: CardUpdateEvent
  }
  deriving (Eq, Show)
instance FromJSON CardUpdate where
  parseJSON = withObject "CardUpdate" $ \o -> do
    update          <- o .: "card"
    crdId           <- update .: "id"
    crdCardholderId <- update .: "cardholder_id"
    crdDesign       <- update .: "design_key"
    crdStatus       <- update .: "status"
    crdLastFour     <- update .: "last_four"
    crdCreated      <- update .: "created_at"
    crdActivated    <- update .: "activated_at"
    crdProgram      <- update .: "program_id"
    crdEvent        <- update .: "event"
    return $ CardUpdate { .. }

parseCardUpdateFromJSON :: CardUpdateEvent -> Value -> Parser CardUpdate
parseCardUpdateFromJSON event = withObject "CardUpdate" $ \o -> do
  update          <- o .: "card"
  crdId           <- update .: "id" <?> Key "card"
  crdCardholderId <- update .: "cardholder_id" <?> Key "card"
  crdDesign       <- update .: "design_key" <?> Key "card"
  crdStatus       <- update .: "status" <?> Key "card"
  crdLastFour     <- update .: "last_four" <?> Key "card"
  crdCreated      <- update .: "created_at" <?> Key "card"
  crdActivated    <- update .: "activated_at" <?> Key "card"
  crdProgram      <- update .: "program_id" <?> Key "card"
  let crdEvent = event
  return $ CardUpdate { .. }

newtype SettlementId = SettlementId Text deriving (Eq, Show, Generic, FromJSON)
data SettlementNotify = SettlementNotify
  { setId            :: SettlementId
  , setDate          :: UTCTime
  , setBalanceTotal  :: Double
  , setBalancePeriod :: Double
  , setCsvUrl        :: Text
  }
  deriving (Eq, Show)
instance FromJSON SettlementNotify where
  parseJSON = withObject "Settlement" $ \o -> do
    sett             <- o .: "settlement"
    setId            <- sett .: "id"
    setDate          <- sett .: "date"
    setBalanceTotal  <- sett .: "total_balance_due"
    setBalancePeriod <- sett .: "period_balance_due"
    setCsvUrl        <- sett .: "csv_url"
    return $ SettlementNotify { .. }
