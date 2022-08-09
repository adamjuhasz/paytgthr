{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.Referral.ReferralCode where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(toField) )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Ids              ( ReferralProgramID
                                                , UserID
                                                )

type ReferralCodeDisplay = Text

data ReferralCode = ReferralCode
  { referrerCode      :: ReferralCodeDisplay
  , codeReferrerId    :: Maybe UserID -- Nothing represents a "promo code"
  , codeProgramLinked :: ReferralProgramID
  , codeCreatedAt     :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow ReferralCode where
  toRow ReferralCode {..} =
    [ toField referrerCode
    , toField codeReferrerId
    , toField codeProgramLinked
    , toField codeCreatedAt
    ]

instance FromRow ReferralCode where
  fromRow = do
    referrerCode      <- field
    codeReferrerId    <- field
    codeProgramLinked <- field
    codeCreatedAt     <- field

    return ReferralCode { .. }

referralCodeFields :: (Query, Query)
referralCodeFields =
  ( " code, referrer_id, program_id, created_at "
  , " ?   , ?          , ?         , ?          "
  )
