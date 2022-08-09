{-# LANGUAGE FlexibleContexts #-}

module APIPrivacy.InternalAPI.CardClose where

import           APIPrivacy.Models.Privacy      ( CardState(..)
                                                , PrivacyCard(..)
                                                )
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
import qualified Data.ByteString.Lazy.Char8    as C
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
import           Shared.WebAPI.ApiPrivacy.API   ( TraceContext )

cardClose
  :: (MonadIO m, HasPrivacyClient m, HasAccounts m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> PrivacyCardToken
  -> m NoContent
cardClose trace userid cardId = do
  userMaybe <- getUser trace userid
  user      <- case userMaybe of
    Nothing -> throwError err404
      { errBody = C.pack $ "No User " <> show (userid, cardId)
      }
    Just u -> return u

  privacyToken <- case usrPrivacyAcctToken user of
    Just t  -> return $ PrivacyAccountToken t
    Nothing -> do
      traceError trace "No usrPrivacyAcctToken for " (userid, trace)
      throwError err500 { errBody = C.pack $ "No User " <> show (userid, cardId)
                        }

  auth <- getPrivacyAuth
  let cardbody = UpdateCardBody { PC.cardToken        = cardId
                                , PC.cardMemo         = Nothing
                                , PC.cardState        = Just CardClosed
                                , PC.cardFundingToken = Nothing
                                , PC.cardPin          = Nothing
                                }

  res <- runPrivacyCmd $ _UpdateCard asClientM auth privacyToken cardbody
  PrivacyCard{} <- case res of
    Right c -> return c
    Left  e -> do
      traceError trace "Error: cardClose _UpdateCard " (userid, e, trace)
      throwError err500
        { errBody = C.pack $ "No User " <> show (userid, cardId, e)
        }

  return NoContent
