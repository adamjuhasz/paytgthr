{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Use lambda-case" -}

module Shared.Models.Card
  ( module Shared.Models.Card
  , CardId(..)
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , withText
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(Escape)
                                                , ToField(..)
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Shared.Models.Base             ( Revision )
import           Shared.Models.Ids              ( CardId(..)
                                                , UserID
                                                )
import           Text.Read                      ( readEither )

-- | Apply a function to a 'Left' constructor
mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left  a1) = Left (f a1)
mapLeft _ (Right b ) = Right b

data CardStatus
  -- | Card has been created but type requires activation
  = CardCreated
  -- | Card is active and be used for purchases
  | CardActive
  -- | Card was frozen by user, no transactions allowed
  | CardUserFrozen
  -- | Card was frozen by Pay Tgthr, no transaction allowed, no user changes allowed
  | CardAdminFrozen
  -- Card is closed, final state. no changed allowd, no transactions allowed
  | CardClosed
  deriving (Eq, Show, Read)
instance FromJSON CardStatus where
  parseJSON = withText "CardStatus" $ \t -> case t of
    "CREATED"     -> return CardCreated
    "NEEDSPIN"    -> return CardActive  -- old state
    "ACTIVATED"   -> return CardActive
    "DEACTIVATED" -> return CardUserFrozen -- old string
    "USERFROZEN"  -> return CardUserFrozen
    "ADMINFROZEN" -> return CardAdminFrozen
    "CLOSED"      -> return CardClosed
    _             -> fail $ "CardStatus doesn't recognize " <> show t
instance ToJSON CardStatus where
  toJSON CardCreated     = "CREATED"
  toJSON CardActive      = "ACTIVATED"
  toJSON CardUserFrozen  = "USERFROZEN"
  toJSON CardAdminFrozen = "ADMINFROZEN"
  toJSON CardClosed      = "CLOSED"
instance ToField CardStatus where
  toField CardCreated     = Escape "created"
  toField CardActive      = Escape "active"
  toField CardUserFrozen  = Escape "userfrozen"
  toField CardAdminFrozen = Escape "adminfrozen"
  toField CardClosed      = Escape "closed"
instance FromField CardStatus where
  fromField a dat = fromField a dat <&> \(t :: String) -> case t of
    "created"     -> CardCreated
    "needspin"    -> CardActive
    "active"      -> CardActive
    "inactive"    -> CardUserFrozen -- old string
    "userfrozen"  -> CardUserFrozen
    "adminfrozen" -> CardAdminFrozen
    "closed"      -> CardClosed
    s             -> error $ "CardStatus: " <> s <> " is not known"
instance ToHttpApiData CardStatus where
  toUrlPiece = toUrlPiece . show
instance FromHttpApiData CardStatus where
  parseUrlPiece = mapLeft T.pack . readEither . T.unpack

data CardDesign
  -- | Apto Card design for mostly yellow cards
  = PinkToYellow
  -- | Apto card drsign for mostly pink cards
  | YellowToPink
  -- | Virtual cards
  | Virtual
  -- | Digital Wallet Cards
  | DigitalWallet
  -- Physical Black Card from Lithic
  | PhysicalBlack
  -- | Catch for unknown 
  | UnknownDesign Text
  deriving (Eq, Show, Read)
instance FromJSON CardDesign where
  parseJSON = withText "CardDesign" $ \t -> case t of
    "pink"          -> return PinkToYellow
    "yellow"        -> return YellowToPink
    "virtual"       -> return Virtual
    "DigitalWallet" -> return DigitalWallet
    "PhysicalBlack" -> return PhysicalBlack
    _               -> return $ UnknownDesign t
instance ToJSON CardDesign where
  toJSON PinkToYellow      = "pink"
  toJSON YellowToPink      = "yellow"
  toJSON Virtual           = "virtual"
  toJSON DigitalWallet     = "DigitalWallet"
  toJSON PhysicalBlack     = "PhysicalBlack"
  toJSON (UnknownDesign t) = toJSON t
instance ToField CardDesign where
  toField PinkToYellow      = Escape "pink"
  toField YellowToPink      = Escape "yellow"
  toField Virtual           = Escape "virtual"
  toField DigitalWallet     = Escape "DigitalWallet"
  toField PhysicalBlack     = Escape "PhysicalBlack"
  toField (UnknownDesign t) = toField t
instance FromField CardDesign where
  fromField a dat = fromField a dat <&> \(t :: String) -> case t of
    "pink"          -> PinkToYellow
    "yellow"        -> YellowToPink
    "virtual"       -> Virtual
    "DigitalWallet" -> DigitalWallet
    "PhysicalBlack" -> PhysicalBlack
    s               -> UnknownDesign $ T.pack s
instance ToHttpApiData CardDesign where
  toUrlPiece = toUrlPiece . show
instance FromHttpApiData CardDesign where
  parseUrlPiece = mapLeft T.pack . readEither . T.unpack

newtype CardLastFour = CardLastFour Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype CardPin = CardPin Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype CardPinEnc = CardPinEnc Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype AptoCardId = AptoCardId Text deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
newtype PrivacyCardToken = PrivacyCardToken Text deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
instance ToHttpApiData PrivacyCardToken where
  toUrlPiece (PrivacyCardToken token) = toUrlPiece token
instance FromHttpApiData PrivacyCardToken where
  parseUrlPiece t = PrivacyCardToken <$> parseUrlPiece t

data IssuerPlatform
  -- | Apto Payments issued card
  = AptoPayments AptoCardId
  -- | Pay with Privacy (privacy.com) issued card
  | PayWithPrivacy PrivacyCardToken
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
instance ToHttpApiData IssuerPlatform where
  toUrlPiece cid = T.pack $ show cid
instance FromHttpApiData IssuerPlatform where
  parseUrlPiece t = case readEither (T.unpack t) of
    Left  e   -> Left $ T.pack e
    Right cid -> Right cid

type CardMemo = Maybe Text

data CardModel = CardModel
  { cardId       :: CardId
  -- ^ Unique id of the card
  , cardPlatform :: IssuerPlatform
  -- ^ Issuer of the card
  , cardRevision :: Revision
  -- ^ monotonimically increasing version to protect stale updated
  , cardDesign   :: CardDesign
  -- ^ Design of the card
  , cardholder   :: UserID
  -- ^ Cardholder
  , cardStatus   :: CardStatus
  -- ^ Current status of the card
  , cardMemo     :: CardMemo
  -- ^ Any commentd about the card
  , createdAt    :: UTCTime
  -- ^ Card creation time
  , activatedAt  :: Maybe UTCTime
  -- ^ Latest card activation time
  , closedAt     :: Maybe UTCTime
  -- ^ When the card was closed
  , updatedAt    :: UTCTime
  -- ^ Last update to the card
  , cardLastFour :: Text
  -- ^ Last 4 digits of the card's PAN
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToRow CardModel where
  toRow CardModel {..} =
    [ toField cardId
    , toField $ case cardPlatform of
      AptoPayments   _ -> "aptopayments" :: Text
      PayWithPrivacy _ -> "paywithprivacy" :: Text
    , toField $ case cardPlatform of
      AptoPayments   (AptoCardId       t) -> t
      PayWithPrivacy (PrivacyCardToken t) -> t
    , toField cardRevision
    , toField cardDesign
    , toField cardholder
    , toField cardStatus
    , toField cardMemo
    , toField createdAt
    , toField activatedAt
    , toField closedAt
    , toField updatedAt
    , toField cardLastFour
    ]

instance FromRow CardModel where
  fromRow = do
    cardId              <- field
    platformTxt :: Text <- field
    platformId :: Text  <- field
    cardRevision        <- field
    cardDesign          <- field
    cardholder          <- field
    cardStatus          <- field
    cardMemo            <- field
    createdAt           <- field
    activatedAt         <- field
    closedAt            <- field
    updatedAt           <- field
    cardLastFour        <- field

    cardPlatform        <- case platformTxt of
      "aptopayments"   -> return $ AptoPayments (AptoCardId platformId)
      "paywithprivacy" -> return $ PayWithPrivacy (PrivacyCardToken platformId)
      t                -> error $ "platformTxt does not recognize " <> show t


    return CardModel { .. }

cardSqlFields :: (Query, Query)
cardSqlFields =
  ( " id, platform, platform_id, revision, design, user_id, status, memo, created_at, activated_at, closed_at, updated_at, last_four "
  , " ?,  ?,        ?,           ?,        ?,      ?,       ?,      ?,    ?,          ?,            ?,         ?,          ? "
  )
