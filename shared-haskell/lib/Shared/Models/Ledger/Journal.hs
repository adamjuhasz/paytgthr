{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.Ledger.Journal
  ( module Shared.Models.Ledger.Journal
  , JournalId
  , PaymentMethod(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
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
import           Shared.Models.Currency         ( Currency
                                                , currencyToDouble
                                                )
import           Shared.Models.Ids              ( JournalId
                                                , LedgerEntryId
                                                , LedgerTrxId
                                                , UserID(..)
                                                )
import           Shared.Models.Payment          ( PaymentMethod(..) )

data JournalSearch
  = GetPayTgthr        UserID
  | GetPayTgthrRewards UserID
  | GetStashTgthr      UserID
  | GetSaveTgthr       UserID
  | GetFundingSource   UserID
  | GetSecurtyDeposit  UserID
  | GetExternalAccount
  | GetVirtualAccount
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data JournalType
  = PayTgthr        UserID
  | PayTgthrRewards UserID
  | StashTgthr      UserID
  | SaveTgthr       UserID
  | SecurtyDeposit  UserID
  | FundingSource   UserID PaymentMethod
  | ExternalAccount        PaymentMethod
  | VirtualAccount
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField JournalType where
  toField = toJSONField
instance FromField JournalType where
  fromField = fromJSONField

searchFor :: JournalType -> JournalSearch
searchFor (PayTgthr        u) = GetPayTgthr u
searchFor (PayTgthrRewards u) = GetPayTgthrRewards u
searchFor (StashTgthr      u) = GetStashTgthr u
searchFor (SaveTgthr       u) = GetSaveTgthr u
searchFor (SecurtyDeposit  u) = GetSecurtyDeposit u
searchFor (FundingSource u _) = GetFundingSource u
searchFor (ExternalAccount _) = GetExternalAccount
searchFor VirtualAccount      = GetVirtualAccount

journalOwner :: JournalType -> Maybe UUID
journalOwner journalType = case journalType of
  PayTgthr        (UserID u) -> Just u
  PayTgthrRewards (UserID u) -> Just u
  StashTgthr      (UserID u) -> Just u
  SaveTgthr       (UserID u) -> Just u
  SecurtyDeposit  (UserID u) -> Just u
  FundingSource (UserID u) _ -> Just u
  ExternalAccount{}          -> Nothing
  VirtualAccount{}           -> Nothing

journalSearchToTag :: JournalSearch -> Text
journalSearchToTag (GetPayTgthr        u) = journalTag $ PayTgthr u
journalSearchToTag (GetPayTgthrRewards u) = journalTag $ PayTgthrRewards u
journalSearchToTag (GetStashTgthr      u) = journalTag $ StashTgthr u
journalSearchToTag (GetSaveTgthr       u) = journalTag $ SaveTgthr u
journalSearchToTag (GetSecurtyDeposit  u) = journalTag $ SecurtyDeposit u
journalSearchToTag (GetFundingSource u) =
  journalTag $ FundingSource u DwollaSettlement
journalSearchToTag GetExternalAccount =
  journalTag $ ExternalAccount DwollaSettlement
journalSearchToTag GetVirtualAccount = journalTag VirtualAccount

journalTag :: JournalType -> Text
journalTag journalType = case journalType of
  PayTgthr{}        -> "PayTgthr"
  PayTgthrRewards{} -> "PayTgthrRewards"
  StashTgthr{}      -> "StashTgthr"
  SaveTgthr{}       -> "SaveTgthr"
  SecurtyDeposit{}  -> "SecurtyDeposit"
  ExternalAccount{} -> "ExternalAccount"
  FundingSource{}   -> "FundingSource"
  VirtualAccount{}  -> "VirtualAccount"

data LedgerJournal = LedgerJournal
  { journalId             :: JournalId
  , journalType           :: JournalType
  , journalName           :: Text
  , journalUser           :: Maybe UserID
  , lastJournalEntry      :: LedgerEntryId
  , journalBalance        :: Currency
  , journalPendingBalance :: Currency
  , journalRevision       :: Int
  , journalCreated        :: UTCTime
  , journalUpdated        :: UTCTime
  , journalTransaction    :: LedgerTrxId
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow LedgerJournal where
  toRow LedgerJournal {..} =
    [ toField journalId
    -- Type is stored split for easier DB analytics
    , toField $ journalTag journalType
    , toField $ journalOwner journalType
    , toField journalType
    , toField journalName
    , toField journalUser
    , toField lastJournalEntry
    , toField journalBalance
    , toField $ currencyToDouble journalBalance
    , toField journalPendingBalance
    , toField $ currencyToDouble journalPendingBalance
    , toField journalRevision
    , toField journalCreated
    , toField journalUpdated
    , toField journalTransaction
    ]

instance FromRow LedgerJournal where
  fromRow = do
    journalId                             <- field
    _journalTypeKind :: Text              <- field
    _journalTypeLink :: Maybe UUID        <- field
    journalType                           <- field
    journalName                           <- field
    journalUser                           <- field
    lastJournalEntry                      <- field
    journalBalance                        <- field
    _journalBalanceDbl :: Rational        <- field
    journalPendingBalance                 <- field
    _journalPendingBalanceDbl :: Rational <- field
    journalRevision                       <- field
    journalCreated                        <- field
    journalUpdated                        <- field
    journalTransaction                    <- field

    return LedgerJournal { .. }

ledgerJournalFields :: (Query, Query)
ledgerJournalFields =
  ( " id, kind, link, type_json, name, user_id, last_entry, balance, balance_number, balance_pending, balance_pending_number, revision, created_at, updated_at, transaction "
  , " ? , ?   , ?   , ?        , ?   , ?      , ?         , ?      , ?             , ?              , ?                     , ?       , ?         , ?         , ?           "
  )
