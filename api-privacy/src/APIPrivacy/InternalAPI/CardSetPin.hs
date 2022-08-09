{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module APIPrivacy.InternalAPI.CardSetPin where

import           APIPrivacy.ExternalAPI.PinEncryption
                                                ( encryptPin )
import           APIPrivacy.Models.Privacy      ( PrivacyCard(..) )
import           APIPrivacy.Monad.HasAccounts   ( HasAccounts(..) )
import           APIPrivacy.Monad.HasPrivacyClient
                                                ( HasPrivacyClient(..) )
import           APIPrivacy.PrivacyClient      as PC
                                                ( Routes(_UpdateCard)
                                                , UpdateCardBody(..)
                                                , asClientM
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Servant                        ( ServerError(..)
                                                , err404
                                                , err500
                                                )
import           Servant.API                    ( NoContent(..) )
import           Shared.Console
import           Shared.Models.Card             ( PrivacyCardToken )
import           Shared.Models.Cardholder       ( PrivacyAccountToken(..) )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.ApiPrivacy.API   ( ChangePinBody(..)
                                                , TraceContext
                                                )
import           System.Random                  ( randomRIO )

data PINBlock = PINBlock
  { nonce :: Integer
  , pin   :: Text
  }
  deriving (Eq, Show)
instance ToJSON PINBlock where
  toJSON PINBlock {..} = object ["nonce" .= nonce, "pin" .= pin]

setCardPin
  :: (MonadIO m, HasPrivacyClient m, HasAccounts m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> PrivacyCardToken
  -> ChangePinBody
  -> m NoContent
setCardPin trace userId cardId ChangePinBody {..} = do
  userMaybe <- getUser trace userId
  user      <- case userMaybe of
    Nothing -> throwError err404
      { errBody = C.pack $ "Error: No User " <> show (userId, cardId)
      }
    Just u -> return u

  privacyToken <- case usrPrivacyAcctToken user of
    Just t  -> return $ PrivacyAccountToken t
    Nothing -> do
      traceError trace
                 "Error: No usrPrivacyAcctToken for "
                 (userId, cardId, trace)
      throwError err500
        { errBody = C.pack $ "Error: No usrPrivacyAcctToken for " <> show
                      (userId, cardId)
        }

  nonce <- liftIO $ randomRIO (11111111, 99999999)
  let block = BL.toStrict . encode $ PINBlock nonce newPinText

  encryptedPin <- TE.decodeUtf8 <$> liftIO (encryptPin block)
  tracePrint trace "encryptedPin: " (userId, cardId, encryptedPin)

  auth <- getPrivacyAuth
  let cardbody = UpdateCardBody { PC.cardToken        = cardId
                                , PC.cardMemo         = Nothing
                                , PC.cardState        = Nothing
                                , PC.cardFundingToken = Nothing
                                , PC.cardPin          = Just encryptedPin
                                }

  res <- runPrivacyCmd $ _UpdateCard asClientM auth privacyToken cardbody
  PrivacyCard{} <- case res of
    Right c -> return c
    Left  e -> do
      traceError trace "Error: setCardPin _UpdateCard " (userId, e, trace)
      throwError err500
        { errBody = C.pack $ "Error: Can't set pin " <> show (userId, cardId, e)
        }

  return NoContent
