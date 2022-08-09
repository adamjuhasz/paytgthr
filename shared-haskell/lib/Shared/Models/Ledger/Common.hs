{-# LANGUAGE StrictData, DeriveGeneric #-}

module Shared.Models.Ledger.Common where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as U
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Stack                      ( HasCallStack )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids              ( PaymentId(..)
                                                , TransactionId(..)
                                                )
import           Shared.Utils                   ( customAesonOptions )

type LedgerIdempotencyKey = Text

data LedgerFact
  = TrxAdjustment TransactionId Currency
  | PaymentCleared PaymentId Currency
  | Manual Currency
  | InitialBalance Currency
  | UserTransfer Currency
  deriving(Eq, Show, Generic)

instance FromJSON LedgerFact where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON LedgerFact where
  toJSON = genericToJSON customAesonOptions
instance ToField LedgerFact where
  toField = toJSONField
instance FromField LedgerFact where
  fromField = fromJSONField

factType :: LedgerFact -> Text
factType TrxAdjustment{}  = "transaction"
factType PaymentCleared{} = "payment"
factType Manual{}         = "manual"
factType InitialBalance{} = "initialBalance"
factType UserTransfer{}   = "UserTransfer"

factAmount :: LedgerFact -> Currency
factAmount (TrxAdjustment  _ curr) = curr
factAmount (PaymentCleared _ curr) = curr
factAmount (Manual         curr  ) = curr
factAmount (InitialBalance curr  ) = curr
factAmount (UserTransfer   curr  ) = curr

factLink :: LedgerFact -> Text
factLink (TrxAdjustment  (TransactionId tid) _) = U.toText tid
factLink (PaymentCleared (PaymentId     pid) _) = U.toText pid
factLink Manual{}                               = ""
factLink InitialBalance{}                       = ""
factLink UserTransfer{}                         = ""

toUUID :: HasCallStack => String -> Maybe Text -> UUID
toUUID err Nothing  = error $ "Error: Should not be empty " <> err
toUUID err (Just t) = fromMaybe (error errMsg) (U.fromText t)
  where errMsg = "Error: Should be UUID " <> err <> " " <> show t

toFact :: String -> Maybe Text -> Currency -> LedgerFact
toFact x@"transaction" uuid amt =
  TrxAdjustment (TransactionId $ toUUID x uuid) amt
toFact x@"adjustment" uuid amt =
  TrxAdjustment (TransactionId $ toUUID x uuid) amt
toFact x@"payment" uuid amt = PaymentCleared (PaymentId $ toUUID x uuid) amt
toFact "manual"         _    amt = Manual amt
toFact "initialBalance" _    amt = InitialBalance amt
toFact "UserTransfer"   _    amt = UserTransfer amt
toFact t                _    _   = error $ "Ledger type unknown \"" <> t <> "\""
