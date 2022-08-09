{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module APIPrivacy.InternalAPI.CardCreate where

import           APIPrivacy.Models.Privacy     as Pri
                                                ( CardState(..)
                                                , CardType(..)
                                                , PrivacyCard(..)
                                                )
import           APIPrivacy.Monad.HasAccounts   ( HasAccounts(..) )
import           APIPrivacy.Monad.HasPrivacyClient
                                                ( HasPrivacyClient(..) )
import           APIPrivacy.PrivacyClient       ( CreateCardBody(..)
                                                , Routes(_CreateCard)
                                                , ShippingAddress(..)
                                                , asClientM
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Servant                        ( ServerError(errBody)
                                                , err404
                                                , err500
                                                )
import           Shared.Console
import           Shared.Models.Card            as Crd
                                                ( CardDesign(..)
                                                , CardStatus(..)
                                                , IssuerPlatform(..)
                                                )
import           Shared.Models.Cardholder       ( PrivacyAccountToken(..) )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.ApiPrivacy.API   ( CardCreateBody(..)
                                                , CardCreatedResponse(..)
                                                , TraceContext
                                                )

cardCreate
  :: (MonadIO m, HasPrivacyClient m, HasAccounts m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> CardCreateBody
  -> m CardCreatedResponse
cardCreate trace userid CardCreateBody {..} = do
  userMaybe      <- getUser trace userid
  UserModel {..} <- case userMaybe of
    Nothing -> do
      traceError trace "Error: No usrPrivacyAcctToken for " (userid, trace)
      throwError err404 { errBody = C.pack $ "Error: No user " <> show userid }
    Just u -> return u

  privacyToken <- case usrPrivacyAcctToken of
    Just t  -> return $ PrivacyAccountToken t
    Nothing -> do
      traceError trace "Error: No usrPrivacyAcctToken for " (userid, trace)
      throwError err404
        { errBody = C.pack $ "Error: No usrPrivacyAcctToken for " <> show
                      (userid, newCardId)
        }

  auth <- getPrivacyAuth
  let cardT = case newCardDesign of
        PinkToYellow    -> DigitalWalletCard
        YellowToPink    -> DigitalWalletCard
        Virtual         -> UnlockedCard
        DigitalWallet   -> DigitalWalletCard
        PhysicalBlack   -> PhysiscalCard
        UnknownDesign _ -> DigitalWalletCard

  let defaultMemo = T.pack $ show (cardT, usrUserID)
  let requestedCardStatus = case cardT of
        SingleUseCard      -> CardOpen
        MerchantLockedCard -> CardOpen
        UnlockedCard       -> CardOpen
        PhysiscalCard      -> CardPaused
        DigitalWalletCard  -> CardOpen
  let
    cardbody = CreateCardBody
      { cardType        = cardT
      , cardMemo        = Just defaultMemo
      , cardState       = requestedCardStatus
      , spendLimit      = Nothing
      , shippingAddress = ShippingAddress
        { shippingFirstName = fromMaybe "" usrFirstName
        , shippingLastName  = fromMaybe "" usrLastName
        , shippingAddress1  = T.replace "," " " $ fromMaybe "" usrAddressStreet
        , shippingAddress2  = T.replace "," " " $ fromMaybe "" usrAddressStreet2
        , shippingCity      = fromMaybe "" usrAddressCity
        , shippingState     = fromMaybe "" usrAddressState
        , shippingZipcode   = fromMaybe "" usrAddressZip
        }
      }

  res <- runPrivacyCmd $ _CreateCard asClientM auth privacyToken cardbody

  tracePrint trace "cardCreate _CreateCard " (userid, res, trace)

  pCard <- case res of
    Right c -> return c
    Left  e -> do
      traceError trace
                 "Error: cardCreate _CreateCard "
                 (userid, newCardId, e, trace)
      throwError err500 { errBody = C.pack $ show (userid, e) }

  return $ CardCreatedResponse
    { createdCardId     = PayWithPrivacy $ cardToken pCard
    , createdCardDesign = newCardDesign
    , createdCardMemo   = Just $ memo pCard
    , createdStatus     = case Pri.cardState pCard of
                            CardOpen               -> CardActive
                            CardPaused             -> CardUserFrozen
                            Pri.CardClosed         -> Crd.CardClosed
                            CardPendingFulfillment -> CardCreated
                            CardPendingActivation  -> CardCreated
    , createdLastFour   = lastFour pCard
    }
