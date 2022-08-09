{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module AFSM.DB.UserGet
  ( getAccountByEmail
  , getAccountByPhone
  , getAccountById
  , getAccountByCardholder
  , getAccountsWithBankInfo
  , getAllActiveAccounts
  , findUsersWithSSN
  , getCardModelByUserId
  , getAccountByCard
  , getSpecficCard
  , getAllAccountsIDs
  , getAllAssesmentsForUser
  , findACard
  , getLastUserNote
  ) where

import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple     ( Connection
                                                , In(..)
                                                , Only(..)
                                                , query
                                                , query_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Card             ( AptoCardId(..)
                                                , CardId(..)
                                                , CardModel
                                                , IssuerPlatform(..)
                                                , PrivacyCardToken(..)
                                                , cardSqlFields
                                                )
import           Shared.Models.Cardholder       ( CardholderId(..)
                                                , PrivacyAccountToken(..)
                                                )
import           Shared.Models.KYCAssesment     ( KYCAssesment
                                                , kycAssesmentFields
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , PhoneNumber(..)
                                                , RedactedText(..)
                                                , UserID(..)
                                                , UserModel
                                                , UserState(..)
                                                , userModelFieldOrder
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

getAccountByEmail :: EmailAddress -> Connection -> IO (Maybe UserModel)
getAccountByEmail (EmailAddress anEmail) conn =
  listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> userModelFieldOrder
      <> " FROM tgthr.users_current_rev WHERE email = ?"
  selector = Only anEmail

getAccountByPhone :: PhoneNumber -> Connection -> IO (Maybe UserModel)
getAccountByPhone (PhoneNumber aNumber) conn =
  listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> userModelFieldOrder
      <> " FROM tgthr.users_current_rev WHERE phone_number = ?"
  selector = Only aNumber

getAccountById :: UserID -> Connection -> IO (Maybe UserModel)
getAccountById (UserID anId) conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> userModelFieldOrder
      <> " FROM tgthr.users_current_rev WHERE id = ?"
  selector = Only anId

getAccountByCardholder :: CardholderId -> Connection -> IO (Maybe UserModel)
getAccountByCardholder cardholderid conn = do
  let textId = case cardholderid of
        AptoPaymentsCH   (AptoCardholderId    t) -> t
        PayWithPrivacyCH (PrivacyAccountToken t) -> t
  listToMaybe <$> query conn qs (Only textId)
 where
  qs =
    "SELECT "
      <> userModelFieldOrder
      <> " FROM tgthr.users_current_rev WHERE apto_cardholderid = ?"

getAccountsWithBankInfo
  :: RedactedText -> RedactedText -> Connection -> IO [UserID]
getAccountsWithBankInfo (RedactedText routing) (RedactedText account) conn =
  fmap fromOnly <$> query
    conn
    [sql| SELECT id FROM tgthr.users_current_rev WHERE bank_routing = ? AND bank_account = ? |]
    (routing, account)

getAllActiveAccounts :: Connection -> IO [UserID]
getAllActiveAccounts conn = fmap fromOnly <$> query_
  conn
  [sql| SELECT id from tgthr.users_current_rev WHERE status = 'active' |]

findUsersWithSSN :: Text -> Connection -> IO [UserID]
findUsersWithSSN ssn conn = do
  uids :: [Only UserID] <- query conn qs selector
  return $ fmap fromOnly uids
 where
  qs       = [sql|SELECT id FROM tgthr.users_current_rev WHERE ssn = ? |]
  selector = Only ssn

getCardModelByUserId :: UserID -> Connection -> IO [CardModel]
getCardModelByUserId (UserID uid) conn = query conn qs $ Only uid
 where
  qs =
    [sql|SELECT |]
      <> fst cardSqlFields
      <> [sql| FROM tgthr.cards_current_rev WHERE user_id = ? |]

getAccountByCard :: IssuerPlatform -> Connection -> IO (Maybe UserID)
getAccountByCard card conn = do
  let textId = case card of
        AptoPayments   (AptoCardId       t) -> t
        PayWithPrivacy (PrivacyCardToken t) -> t
  res :: [Only UserID] <- query conn qs (Only textId)
  return . listToMaybe $ fmap fromOnly res
 where
  qs =
    [sql| SELECT user_id FROM tgthr.cards_current_rev WHERE platform_id = ? |]

getSpecficCard :: CardId -> Connection -> IO (Maybe CardModel)
getSpecficCard (CardId cardId) conn = listToMaybe
  <$> query conn qs (Only cardId)
 where
  qs =
    [sql| SELECT |]
      <> fst cardSqlFields
      <> [sql| FROM tgthr.cards_current_rev WHERE id = ? |]

getAllAccountsIDs :: [UserState] -> Connection -> IO [UserID]
getAllAccountsIDs states conn = do
  ids <- query
    conn
    [sql| SELECT id FROM tgthr.users_current_rev WHERE status in ? |]
    (Only $ In states)
  putStrLn $ "getAllAccountsIDs res count: " <> show (states, length ids)
  return $ fmap fromOnly ids

getAllAssesmentsForUser
  :: TraceContext -> UserID -> Connection -> IO [KYCAssesment]
getAllAssesmentsForUser _trace user conn = query conn qs $ Only user
 where
  qs =
    [sql| SELECT |]
      <> fst kycAssesmentFields
      <> [sql| FROM tgthr.kyc_assesments WHERE user_id = ? ORDER BY created_at DESC |]

findACard :: IssuerPlatform -> Connection -> IO (Maybe CardModel)
findACard issuerId conn = listToMaybe <$> query conn qs (Only platform_id)
 where
  (_platform_text :: Text, platform_id :: Text) = case issuerId of
    AptoPayments   (AptoCardId       platId) -> ("aptopayments", platId)
    PayWithPrivacy (PrivacyCardToken platId) -> ("paywithprivacy", platId)
  qs =
    [sql| SELECT |]
      <> fst cardSqlFields
      <> [sql| FROM tgthr.cards_current_rev WHERE platform_id = ? |]

type Revision = Int
getLastUserNote :: UserID -> Connection -> IO (Maybe (Text, Revision))
getLastUserNote uid conn = listToMaybe <$> query
  conn
  [sql| 
    SELECT note, revision 
    FROM tgthr.user_notes_current_rev 
    WHERE user_id = ? 
  |]
  (Only uid)
