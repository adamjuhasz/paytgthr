{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{- HLINT ignore "Reduce duplication" -}

module APIPrivacy.InternalAPI.CardHostedUI where

import           APIPrivacy.Monad.HasAccounts   ( HasAccounts(getUser) )
import           APIPrivacy.Monad.HasPrivacyClient
                                                ( HasPrivacyClient(..) )
import           APIPrivacy.PrivacyClient       ( Routes(_PCIUI)
                                                , asClientM
                                                , runClientM
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Crypto.Hash.Algorithms         ( SHA256 )
import           Crypto.MAC.HMAC                ( HMAC(hmacGetDigest)
                                                , hmac
                                                )
import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , encode
                                                , object
                                                )
import           Data.ByteArray.Encoding        ( Base(Base64)
                                                , convertToBase
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text
                                                , pack
                                                , strip
                                                , unpack
                                                )
import qualified Data.Text.Encoding            as TE
import           Servant                        ( ServerError(..)
                                                , err404
                                                , err500
                                                )
import           Servant.Client                 ( ClientEnv )
import           Shared.Models.Card             ( PrivacyCardToken )
import           Shared.Models.Cardholder       ( PrivacyAccountToken(..) )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.ApiPrivacy.API   ( CardPciInfo(..)
                                                , TraceContext
                                                )
import qualified Text.HandsomeSoup             as HS
import           Text.XML.HXT.Core              ( (>>>)
                                                , ArrowTree((/>))
                                                , ArrowXml(getText)
                                                , runX
                                                )

getHostedUIURL
  :: (MonadIO m, HasPrivacyClient m, HasAccounts m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> PrivacyCardToken
  -> m Text
getHostedUIURL trace userId cardId = do
  apiKey    <- getPrivacyAPIKey
  urlBase   <- getPrivacyURLBase

  userMaybe <- getUser trace userId
  user      <- case userMaybe of
    Just u  -> return u
    Nothing -> throwError err404
      { errBody = C.pack $ "Error: No user " <> show (userId, cardId)
      }
  accountToken <- case usrPrivacyAcctToken user of
    Just a  -> return $ PrivacyAccountToken a
    Nothing -> throwError err500
      { errBody = C.pack $ "Error: No usrPrivacyAcctToken " <> show
                    (userId, cardId)
      }

  generateURL urlBase apiKey accountToken cardId

getCardInfo
  :: (MonadIO m, HasPrivacyClient m, HasAccounts m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> PrivacyCardToken
  -> m CardPciInfo
getCardInfo trace userId cardId = do
  env       <- getPrivacyEnv
  apiKey    <- getPrivacyAPIKey

  userMaybe <- getUser trace userId
  user      <- case userMaybe of
    Just u  -> return u
    Nothing -> throwError err404
      { errBody = C.pack $ "Error: No user " <> show (userId, cardId)
      }

  accountToken <- case usrPrivacyAcctToken user of
    Just a  -> return $ PrivacyAccountToken a
    Nothing -> throwError err500
      { errBody = C.pack $ "Error: No usrPrivacyAcctToken " <> show
                    (userId, cardId)
      }

  let hashes = hashTokens apiKey accountToken cardId
  (pan, expMonth, expShortYear, cvv) <- parseHostedUI env hashes
  return CardPciInfo { pan, expMonth, expShortYear, cvv }

base64Digest :: HMAC SHA256 -> ByteString
base64Digest = convertToBase Base64 . hmacGetDigest

data EmbedRequest = EmbedRequest
  { token        :: PrivacyCardToken
  , css          :: Text
  , accountToken :: PrivacyAccountToken
  }
  deriving (Eq, Show)
instance ToJSON EmbedRequest where
  toJSON EmbedRequest {..} =
    object ["css" .= css, "token" .= token, "account_token" .= accountToken]

generateURL
  :: (MonadIO m)
  => Text
  -> Text
  -> PrivacyAccountToken
  -> PrivacyCardToken
  -> m Text
generateURL urlBase apiKey accountToken cardId = do
  let (embedB64, digest) = hashTokens apiKey accountToken cardId

  return
    $  urlBase
    <> "/v1/embed/card?embed_request="
    <> embedB64
    <> "&hmac="
    <> digest

type EmbedRrequestBase64 = Text
type EBHmacDigest = Text

hashTokens
  :: Text
  -> PrivacyAccountToken
  -> PrivacyCardToken
  -> (EmbedRrequestBase64, EBHmacDigest)
hashTokens apiKey accountToken cardId =
  let embed = EmbedRequest cardId
                           "https://paytgthr.com/css/privacycard.css"
                           accountToken
      jsonStr                = BSL.toStrict $ encode embed
      embedB64 :: ByteString = convertToBase Base64 jsonStr
      apiKeyBs               = TE.encodeUtf8 apiKey
      hm                     = hmac apiKeyBs jsonStr
      digest                 = base64Digest hm
  in  (TE.decodeUtf8 embedB64, TE.decodeUtf8 digest)

type PAN = Text
type ExpMonth = Text
type ExpShortYear = Text
type CVV = Text

parsePCIHtml :: (MonadIO m) => String -> m (Maybe (Text, Text, Text, Text))
parsePCIHtml html = do
  let parsed = HS.parseHtml html
  pans   <- liftIO $ runX $ parsed >>> HS.css "#pan" /> getText
  months <- liftIO $ runX $ parsed >>> HS.css "#month" /> getText
  years  <- liftIO $ runX $ parsed >>> HS.css "#year" /> getText
  cvvs   <- liftIO $ runX $ parsed >>> HS.css "#cvv" /> getText

  case
      ( strip . pack . concat $ pans
      , strip . pack . concat $ months
      , strip . pack . concat $ years
      , strip . pack . concat $ cvvs
      )
    of
      ("", "", "", "") -> do
        liftIO $ putStr "Error: parse HTML as empty " >> print html
        return Nothing
      ("", _, _, _) -> do
        liftIO $ putStr "Error: parse PAN as empty " >> print html
        return Nothing
      (_, "", _, _) -> do
        liftIO $ putStr "Error: parse Month as empty " >> print html
        return Nothing
      (_, _, "", _) -> do
        liftIO $ putStr "Error: parse Year as empty " >> print html
        return Nothing
      (_, _, _, "") -> do
        liftIO $ putStr "Error: parse CVV as empty " >> print html
        return Nothing
      (pan, month, year, cvv) -> return $ Just (pan, month, year, cvv)

parseHostedUI
  :: (MonadIO m, MonadError ServerError m)
  => ClientEnv
  -> (EmbedRrequestBase64, EBHmacDigest)
  -> m (PAN, ExpMonth, ExpShortYear, CVV)
parseHostedUI privacyEnv (embedB64, digest) = do
  let fn = _PCIUI asClientM embedB64 digest
  res  <- liftIO $ runClientM fn privacyEnv

  html <- case res of
    Right h -> return $ unpack h
    Left  e -> do
      liftIO $ putStr "Error: parseHostedUI _PCIUI " >> print e
      throwError err500

  parseRes                <- parsePCIHtml html
  (pan, month, year, cvv) <- case parseRes of
    Just (pan, month, year, cvv) -> return (pan, month, year, cvv)
    Nothing                      -> throwError err500

  return (pan, month, year, cvv)
