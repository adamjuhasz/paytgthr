{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shared.Models.Ids where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.UUID                      ( UUID
                                                , toText
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData(parseUrlPiece)
                                                , ToHttpApiData(toUrlPiece)
                                                )

newtype LedgerEntryId = LedgerEntryId UUID deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)
instance ToField LedgerEntryId where
  toField (LedgerEntryId anID) = toField anID
instance FromField LedgerEntryId where
  fromField a dat = LedgerEntryId <$> fromField a dat

newtype JournalId = JournalId UUID deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)
instance ToField JournalId where
  toField (JournalId anID) = toField anID
instance FromField JournalId where
  fromField a dat = JournalId <$> fromField a dat
instance FromHttpApiData JournalId where
  parseUrlPiece t = JournalId <$> parseUrlPiece t
instance ToHttpApiData JournalId where
  toUrlPiece (JournalId gid) = toText gid

newtype LedgerTrxId = LedgerTrxId UUID deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)
instance ToField LedgerTrxId where
  toField (LedgerTrxId anID) = toField anID
instance FromField LedgerTrxId where
  fromField a dat = LedgerTrxId <$> fromField a dat

newtype PaymentId = PaymentId UUID deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField PaymentId where
  toField (PaymentId u) = toField u
instance FromField PaymentId where
  fromField a dat = PaymentId <$> fromField a dat
instance ToHttpApiData PaymentId where
  toUrlPiece (PaymentId uid) = toText uid
instance FromHttpApiData PaymentId where
  parseUrlPiece t = PaymentId <$> parseUrlPiece t

newtype MessageID = MessageID UUID deriving (Show, Eq, Generic, FromJSON, ToJSON)
instance ToField MessageID where
  toField (MessageID uuid) = toField uuid
instance FromField MessageID where
  fromField a dat = MessageID <$> fromField a dat
instance FromHttpApiData MessageID where
  parseUrlPiece t = MessageID <$> parseUrlPiece t

newtype UserID = UserID UUID deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)
instance ToField UserID where
  toField (UserID u) = toField u
instance FromField UserID where
  fromField f mbs = UserID <$> fromField f mbs
instance FromHttpApiData UserID where
  parseUrlPiece t = UserID <$> parseUrlPiece t
instance ToHttpApiData UserID where
  toUrlPiece (UserID uid) = toText uid

newtype TransactionId = TransactionId UUID deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToField TransactionId where
  toField (TransactionId uuid) = toField uuid
instance FromField TransactionId where
  fromField f mbs = TransactionId <$> fromField f mbs
instance FromHttpApiData TransactionId where
  parseUrlPiece t = TransactionId <$> parseUrlPiece t
instance ToHttpApiData TransactionId where
  toUrlPiece (TransactionId tid) = toText tid

newtype GroupId = GroupId UUID deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
instance ToField GroupId where
  toField (GroupId anID) = toField anID
instance FromField GroupId where
  fromField a dat = GroupId <$> fromField a dat
instance FromHttpApiData GroupId where
  parseUrlPiece t = GroupId <$> parseUrlPiece t
instance ToHttpApiData GroupId where
  toUrlPiece (GroupId gid) = toText gid

newtype CardId = CardId UUID deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField CardId where
  toField (CardId u) = toField u
instance FromField CardId where
  fromField f mbs = CardId <$> fromField f mbs
instance ToHttpApiData CardId where
  toUrlPiece (CardId token) = toUrlPiece token
instance FromHttpApiData CardId where
  parseUrlPiece t = CardId <$> parseUrlPiece t

newtype RewardId = RewardId UUID deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)
instance ToField RewardId where
  toField (RewardId u) = toField u
instance FromField RewardId where
  fromField f mbs = RewardId <$> fromField f mbs
instance ToHttpApiData RewardId where
  toUrlPiece (RewardId token) = toUrlPiece token
instance FromHttpApiData RewardId where
  parseUrlPiece t = RewardId <$> parseUrlPiece t

newtype ActivatedRewardId = ActivatedRewardId UUID deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField ActivatedRewardId where
  toField (ActivatedRewardId u) = toField u
instance FromField ActivatedRewardId where
  fromField f mbs = ActivatedRewardId <$> fromField f mbs
instance ToHttpApiData ActivatedRewardId where
  toUrlPiece (ActivatedRewardId token) = toUrlPiece token
instance FromHttpApiData ActivatedRewardId where
  parseUrlPiece t = ActivatedRewardId <$> parseUrlPiece t

newtype ReferralProgramID = ReferralProgramID UUID deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField ReferralProgramID where
  toField (ReferralProgramID u) = toField u
instance FromField ReferralProgramID where
  fromField f mbs = ReferralProgramID <$> fromField f mbs
instance ToHttpApiData ReferralProgramID where
  toUrlPiece (ReferralProgramID token) = toUrlPiece token
instance FromHttpApiData ReferralProgramID where
  parseUrlPiece t = ReferralProgramID <$> parseUrlPiece t

newtype ReferralProgressID = ReferralProgressID UUID deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField ReferralProgressID where
  toField (ReferralProgressID u) = toField u
instance FromField ReferralProgressID where
  fromField f mbs = ReferralProgressID <$> fromField f mbs
instance ToHttpApiData ReferralProgressID where
  toUrlPiece (ReferralProgressID token) = toUrlPiece token
instance FromHttpApiData ReferralProgressID where
  parseUrlPiece t = ReferralProgressID <$> parseUrlPiece t
