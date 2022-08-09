{- HLINT ignore "Use let" -}
{-# LANGUAGE RecordWildCards, StrictData, DeriveGeneric #-}

module Shared.Models.Ledger.Entry where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Data.Time.Clock                ( UTCTime )
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
                                                ( Null(Null)
                                                , Query
                                                )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Currency         ( Currency(..)
                                                , currencyToDouble
                                                )
import           Shared.Models.Ids              ( JournalId
                                                , LedgerEntryId(..)
                                                , LedgerTrxId
                                                , MessageID
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact
                                                , LedgerIdempotencyKey
                                                , factAmount
                                                , factLink
                                                , factType
                                                , toFact
                                                )
import           Shared.Utils                   ( customAesonOptions )

data LedgerEntry = LedgerEntry
  { lenId             :: LedgerEntryId -- Primary
  , lenJournal        :: JournalId
  , lenUser           :: Maybe UserID
  , lenRevision       :: Integer
  , lenVersion        :: Text
  , lenMsgSource      :: MessageID
  , lenBalance        :: Currency
  , lenPendingBalance :: Currency
  , lenFact           :: LedgerFact -- Split in DB to `type` & `amount` & `typeID`
  , lenIdempotency    :: Maybe LedgerIdempotencyKey
  , lenCreatedAt      :: UTCTime
  , lenTransaction    :: Maybe LedgerTrxId
  }
  deriving (Eq, Show, Generic)

defaultEntry
  :: UserID
  -> MessageID
  -> Maybe LedgerIdempotencyKey
  -> LedgerFact
  -> UTCTime
  -> LedgerEntryId
  -> JournalId
  -> LedgerEntry
defaultEntry uid mid idem fact now lid jid = LedgerEntry
  { lenId             = lid
  , lenRevision       = 1
  , lenVersion        = "1.0"
  , lenMsgSource      = mid
  , lenUser           = Just uid
  , lenBalance        = factAmount fact
  , lenPendingBalance = Currency "USD" 0
  , lenFact           = fact
  , lenIdempotency    = idem
  , lenCreatedAt      = now
  , lenJournal        = jid
  , lenTransaction    = Nothing
  }

instance FromJSON LedgerEntry where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON LedgerEntry where
  toJSON = genericToJSON customAesonOptions
instance ToRow LedgerEntry where
  toRow LedgerEntry {..} =
    [ toField lenId
    , toField lenRevision
    , toField lenVersion
    , toField lenMsgSource
    , toField lenUser
    , toField lenBalance
    , toField . Escape . TE.encodeUtf8 . factType $ lenFact
    , case factLink lenFact of
      "" -> toField Null
      t  -> toField t
    , toField $ factAmount lenFact
    , toField lenIdempotency
    , toField lenCreatedAt
    , toField lenJournal
    , toField lenPendingBalance
    , toField $ currencyToDouble lenPendingBalance
    , toField lenTransaction
    ]

instance FromRow LedgerEntry where
  fromRow = do
    lenId                          <- field
    lenRevision                    <- field
    lenVersion                     <- field
    lenMsgSource                   <- field
    lenUser                        <- field
    lenBalance                     <- field
    aType :: String                <- field
    factId :: Maybe Text           <- field
    factAmt                        <- field
    lenIdempotency                 <- field
    lenCreatedAt                   <- field
    lenJournal                     <- field
    lenPendingBalance              <- field
    _pendingBalanceDbl :: Rational <- field
    lenTransaction                 <- field

    let lenFact = toFact aType factId factAmt

    return LedgerEntry { .. }


ledgerEntryFields :: (Query, Query)
ledgerEntryFields =
  ( " id, revision, version, msg_source, user_id, balance, type, type_id, amount, idempotency, created_at, journal_id, balance_pending, balance_pending_number, transaction_id "
  , " ? , ?       , ?      , ?         , ?      , ?      , ?   , ?      , ?     , ?          , ?         , ?         , ?              , ?                     , ?              "
  )
