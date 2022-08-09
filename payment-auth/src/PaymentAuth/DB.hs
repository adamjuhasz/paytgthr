{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PaymentAuth.DB where

import           Control.Monad                  ( void )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(..)
                                                , execute
                                                , query
                                                , query_
                                                )
import           Shared.Models.PaymentAuth      ( PlaidBalanceRow(..)
                                                , PlaidTokenRow(..)
                                                , UserID
                                                , defaultPlaidTokenRow
                                                , plaidBalanceRowFieldOrder
                                                , plaidTokenRowFieldOrder
                                                )
import           Shared.Models.Plaid.Base       ( AccessToken
                                                , Account
                                                , ItemId(..)
                                                , PlaidAccountId
                                                , PlaidEnvironment
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID )

insertBalance
  :: MessageID -> UserID -> (Account, Double) -> Connection -> IO ()
insertBalance mid user (acct, time) conn = void $ execute
  conn
  (  "INSERT INTO tgthr.balances ("
  <> fst plaidBalanceRowFieldOrder
  <> ") VALUES ("
  <> snd plaidBalanceRowFieldOrder
  <> ")"
  )
  row
  where row = PlaidBalanceRow user time mid acct

getTokens :: Connection -> IO [(AccessToken, PlaidEnvironment)]
getTokens conn =
  query_ conn "SELECT token, plaid_environment FROM tgthr.plaidtokens"

getPrimaryAccount :: UserID -> Connection -> IO (Maybe PlaidAccountId)
getPrimaryAccount user conn = do
  res :: [Only (Maybe PlaidAccountId)] <- query
    conn
    "SELECT account_primary FROM tgthr.plaidtokens WHERE user_id = ? AND account_primary IS NOT NULL ORDER BY revision DESC LIMIT 1"
    (Only user)
  return $ case listToMaybe res of
    Nothing       -> Nothing
    Just (Only x) -> x

getAccessToken
  :: UserID -> Connection -> IO (Maybe (AccessToken, PlaidEnvironment))
getAccessToken (UserID uid) conn =
  query
      conn
      "SELECT token, plaid_environment FROM tgthr.plaidtokens WHERE user_id = ? ORDER BY revision DESC LIMIT 1"
      (Only uid)
    <&> listToMaybe

getPlaidTokenRow :: UserID -> Connection -> IO (Maybe PlaidTokenRow)
getPlaidTokenRow uid conn = listToMaybe <$> query
  conn
  (  "SELECT "
  <> fst plaidTokenRowFieldOrder
  <> " FROM tgthr.plaidtokens WHERE user_id = ? ORDER BY revision DESC LIMIT 1"
  )
  (Only uid)

insertPlaidTokenRow :: PlaidTokenRow -> Connection -> IO ()
insertPlaidTokenRow row conn = void $ execute
  conn
  (  "INSERT INTO tgthr.plaidtokens ("
  <> fst plaidTokenRowFieldOrder
  <> ") VALUES ("
  <> snd plaidTokenRowFieldOrder
  <> ")"
  )
  row

type ABANumber = Text
type DDANumber = Text
updateTokenPrimary
  :: MessageID
  -> UserID
  -> (PlaidAccountId, ABANumber, DDANumber)
  -> Connection
  -> IO ()
updateTokenPrimary mid uid (aID, abaNumber, ddaNumber) conn =
  void
    $   getPlaidTokenRow uid conn
    <&> fromJust
    <&> (\r@PlaidTokenRow {..} -> r { accountPrimary = Just aID
                                    , msgSource      = mid
                                    , revision       = revision + 1
                                    , accountABA     = Just abaNumber
                                    , accountDDA     = Just ddaNumber
                                    }
        )
    >>= (`insertPlaidTokenRow` conn)

insertToken
  :: MessageID
  -> UserID
  -> (AccessToken, ItemId, PlaidEnvironment)
  -> Connection
  -> IO ()
insertToken mid uid (token, iid, env) conn = do
  currRow <- getPlaidTokenRow uid conn
  let futureRow = case currRow of
        Nothing -> defRow { revision = 1 }
        Just r  -> defRow { revision = revision r + 1 }
  insertPlaidTokenRow futureRow conn
  where defRow = defaultPlaidTokenRow token uid env mid iid

getRecentBalanceSince
  :: UserID -> UTCTime -> Connection -> IO (Maybe (Rational, UTCTime))
getRecentBalanceSince (UserID uid) since conn = do
  res :: Maybe (Rational, Maybe Rational, UTCTime) <- listToMaybe <$> query
    conn
    "SELECT balance_current, balance_available, created_at FROM tgthr.balances WHERE user_id = ? AND created_at >= ? ORDER BY created_at DESC LIMIT 1"
    (uid, since)
  return
    $   res
    <&> (\(current, available, accessedAt) ->
          (min current (fromMaybe current available), accessedAt)
        )


getUserFromItem :: ItemId -> Connection -> IO (Maybe UserID)
getUserFromItem (ItemId iID) conn = do
  results <- query
    conn
    "SELECT user_id FROM tgthr.plaidtokens WHERE plaid_item_id = ? ORDER BY revision DESC LIMIT 1"
    (Only iID)
  let maybeUser = results & fmap fromOnly & fmap UserID & listToMaybe
  return maybeUser

