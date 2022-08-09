{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shared.Models.Ledger.Transaction where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )

import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Currency         ( getMonetaryValue )
import           Shared.Models.Ids              ( LedgerEntryId
                                                , LedgerTrxId
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact
                                                , LedgerIdempotencyKey
                                                , factAmount
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )

data TransactionEntries
  = EntriesById [LedgerEntryId ]
  | Entries [LedgerEntry]
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToField [LedgerEntryId] where
  toField = toJSONField
instance FromField [LedgerEntryId] where
  fromField = fromJSONField

data LedgerTransaction = LedgerTransaction
  { ltxId          :: LedgerTrxId
  , ltxEntries     :: TransactionEntries
  , ltxCreated     :: UTCTime
  , ltxUpdated     :: UTCTime
  , ltxFact        :: LedgerFact
  , ltxIdempotency :: Maybe LedgerIdempotencyKey
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToRow LedgerTransaction where
  toRow LedgerTransaction {..} =
    [ toField ltxId
    , toField $ case ltxEntries of
      EntriesById ids     -> ids
      Entries     entries -> fmap lenId entries
    , toField ltxCreated
    , toField ltxUpdated
    , toField ltxFact
    , toField ltxIdempotency
    ]

instance FromRow LedgerTransaction where
  fromRow = do
    ltxId          <- field
    ltxEntries     <- EntriesById <$> field
    ltxCreated     <- field
    ltxUpdated     <- field
    ltxFact        <- field
    ltxIdempotency <- field

    return LedgerTransaction { .. }

ledgerTransactionlFields :: (Query, Query)
ledgerTransactionlFields =
  ( " id, entries_id, created_at, updated_at, fact, idempotency "
  , " ? , ?         , ?         , ?         , ?   , ?           "
  )

verifyTransactionIsSound :: LedgerTransaction -> Either String ()
verifyTransactionIsSound LedgerTransaction { ltxEntries = EntriesById{} } =
  Right ()
verifyTransactionIsSound trx@LedgerTransaction { ltxEntries = Entries entries }
  = let amounts = fmap (factAmount . lenFact) entries
        total   = sum amounts
    in  if getMonetaryValue total == 0
          then Right ()
          else Left $ "Error: LedgerTransaction sum is not 0 " <> show trx
