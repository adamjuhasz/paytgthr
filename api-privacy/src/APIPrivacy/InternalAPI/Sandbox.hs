{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Reduce duplication" -}

module APIPrivacy.InternalAPI.Sandbox where

import           APIPrivacy.Monad.HasAccounts   ( HasAccounts(..) )
import           APIPrivacy.Monad.HasPrivacyClient
                                                ( HasPrivacyClient(..) )
import           APIPrivacy.PrivacyClient      as PC
                                                ( Routes
                                                  ( _SandboxSimAuthorization
                                                  , _SandboxSimClearing
                                                  , _SandboxSimReturn
                                                  , _SandboxSimVoid
                                                  )
                                                , SandboxAuthorization
                                                  ( SandboxAuthorization
                                                  )
                                                , SandboxAuthorizationResponse
                                                  ( SandboxAuthorizationResponse
                                                  , token
                                                  )
                                                , SandboxVoidClearing
                                                  ( SandboxVoidClearing
                                                  )
                                                , TransactionToken
                                                  ( TransactionToken
                                                  )
                                                , asClientM
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(Down) )
import           Data.Text                     as T
                                                ( Text
                                                , take
                                                )
import           Data.UUID                      ( toText )
import           Data.UUID.V4                   ( nextRandom )
import           Servant                        ( NoContent(NoContent)
                                                , ServerError
                                                , err500
                                                )
import           Shared.Models.Card            as Card
                                                ( CardModel
                                                  ( CardModel
                                                  , cardMemo
                                                  , cardPlatform
                                                  , createdAt
                                                  )
                                                , IssuerPlatform(..)
                                                )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.User             ( RedactedText(..)
                                                , UserID
                                                )
import           Shared.WebAPI.ApiPrivacy.API   ( TraceContext )

isPrivacyCard :: CardModel -> Bool
isPrivacyCard CardModel { cardPlatform = PayWithPrivacy _, cardMemo = Just _ }
  = True
isPrivacyCard _ = False

sandboxAuth
  :: (HasAccounts m, HasPrivacyClient m, MonadIO m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> Double
  -> m Text
sandboxAuth trace userId simAmount = do
  cards <-
    sortOn (Data.Ord.Down . createdAt)
    .   filter isPrivacyCard
    <$> getCardsFor trace userId
  aCard <- case cards of
    card : _ -> return card
    []       -> do
      liftIO $ putStr "sandboxAuth user has not cards " >> print (userId, trace)
      throwError err500

  liftIO $ putStr "Using card for sandboxAuth " >> print (userId, aCard, trace)

  cardPAN <- case Card.cardMemo aCard of
    Just cardpan -> return cardpan
    Nothing      -> do
      liftIO $ putStr "sandboxAuth user's card has no pan " >> print
        (userId, cards, aCard, trace)
      throwError err500

  liftIO $ putStr "Pan for card for sandboxAuth " >> print
    (cardPAN, userId, aCard, trace)

  trxName <- T.take 24 . ("TRX-" <>) . toText <$> liftIO nextRandom
  liftIO $ putStr "sandboxAuth Trx name " >> print (trxName, userId, cardPAN)

  auth <- getPrivacyAuth
  let fn = _SandboxSimAuthorization asClientM auth $ SandboxAuthorization
        trxName
        (RedactedText cardPAN)
        (Currency "USD" $ toRational simAmount)


  res <- runPrivacyCmd fn
  liftIO $ putStr "sandboxAuth _SandboxSimAuthorization res " >> print
    (res, trxName, userId, cardPAN)

  case res of
    Right SandboxAuthorizationResponse { token = TransactionToken tok } ->
      return tok
    Left e -> do
      liftIO $ putStr "sandboxAuth _SandboxSimAuthorization " >> print
        (userId, e)
      throwError err500

sandboxVoid
  :: (HasPrivacyClient m, MonadIO m, MonadError ServerError m)
  => TraceContext
  -> Text
  -> Double
  -> m NoContent
sandboxVoid _trace trxToken voidAmt = do
  auth <- getPrivacyAuth
  let fn = _SandboxSimVoid asClientM auth $ SandboxVoidClearing
        (TransactionToken trxToken)
        (Currency "USD" $ toRational voidAmt)

  res <- runPrivacyCmd fn
  liftIO $ putStr "sandboxAuth _SandboxSimVoid res " >> print (res, trxToken)

  case res of
    Right _ -> return NoContent
    Left  e -> do
      liftIO $ putStr "sandboxVoid _SandboxSimVoid " >> print (trxToken, e)
      throwError err500

sandboxClearing
  :: (HasPrivacyClient m, MonadIO m, MonadError ServerError m)
  => TraceContext
  -> Text
  -> Double
  -> m NoContent
sandboxClearing _trace trxToken voidAmt = do
  auth <- getPrivacyAuth
  let fn = _SandboxSimClearing asClientM auth $ SandboxVoidClearing
        (TransactionToken trxToken)
        (Currency "USD" $ toRational voidAmt)

  res <- runPrivacyCmd fn
  liftIO $ putStr "sandboxAuth _SandboxSimClearing res " >> print
    (res, trxToken)

  case res of
    Right _ -> return NoContent
    Left  e -> do
      liftIO $ putStr "sandboxClearing _SandboxSimClearing " >> print
        (trxToken, e)
      throwError err500

sandboxReturn
  :: (HasAccounts m, HasPrivacyClient m, MonadIO m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> Double
  -> m Text
sandboxReturn trace userId simAmount = do
  cards <-
    sortOn (Data.Ord.Down . createdAt)
    .   filter isPrivacyCard
    <$> getCardsFor trace userId
  aCard <- case cards of
    card : _ -> return card
    []       -> do
      liftIO $ putStr "sandboxReturn user has not cards " >> print
        (userId, trace)
      throwError err500

  cardPAN <- case Card.cardMemo aCard of
    Just cardpan -> return cardpan
    Nothing      -> do
      liftIO $ putStr "sandboxReturn user's card has no pan " >> print
        (userId, cards, aCard, trace)
      throwError err500

  trxName <- T.take 24 . ("RTN-" <>) . toText <$> liftIO nextRandom
  liftIO $ putStr "sandboxAuth Return name " >> print (trxName, userId, cardPAN)

  auth <- getPrivacyAuth
  let fn = _SandboxSimReturn asClientM auth $ SandboxAuthorization
        trxName
        (RedactedText cardPAN)
        (Currency "USD" $ toRational simAmount)

  res <- runPrivacyCmd fn
  liftIO $ putStr "sandboxAuth _SandboxSimReturn res " >> print
    (res, trxName, userId, cardPAN)

  case res of
    Right SandboxAuthorizationResponse { token = TransactionToken tok } ->
      return tok
    Left e -> do
      liftIO $ putStr "sandboxReturn _SandboxSimReturn " >> print (userId, e)
      throwError err500
