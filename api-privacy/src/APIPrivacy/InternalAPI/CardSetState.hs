{-# LANGUAGE FlexibleContexts #-}


module APIPrivacy.InternalAPI.CardSetState
  ( setCardState
  ) where

import           APIPrivacy.Models.Privacy     as Privacy
                                                ( CardState(..)
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
                                                , err401
                                                , err404
                                                , err500
                                                )
import           Servant.API                    ( NoContent(..) )
import           Shared.Console
import           Shared.Models.Card            as Tgthr
                                                ( CardStatus(..)
                                                , PrivacyCardToken
                                                )
import           Shared.Models.Cardholder       ( PrivacyAccountToken(..) )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

setCardState
  :: (MonadIO m, HasPrivacyClient m, HasAccounts m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> PrivacyCardToken
  -> CardStatus
  -> m NoContent
setCardState trace userId cardId newStatus = do
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

  privacyCardState <- case newStatus of
    CardCreated      -> throwError err401
    CardActive       -> return CardOpen
    CardUserFrozen   -> return CardPaused
    CardAdminFrozen  -> return CardPaused
    Tgthr.CardClosed -> return Privacy.CardClosed

  auth <- getPrivacyAuth
  let cardbody = UpdateCardBody { PC.cardToken        = cardId
                                , PC.cardMemo         = Nothing
                                , PC.cardState        = Just privacyCardState
                                , PC.cardFundingToken = Nothing
                                , PC.cardPin          = Nothing
                                }

  res <- runPrivacyCmd $ _UpdateCard asClientM auth privacyToken cardbody
  PrivacyCard{} <- case res of
    Right c -> return c
    Left  e -> do
      traceError trace "Error: setCardState _UpdateCard " (userId, e, trace)
      throwError err500

  return NoContent
