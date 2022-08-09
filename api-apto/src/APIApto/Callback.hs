{-# LANGUAGE StrictData, RecordWildCards #-}

module APIApto.Callback
  ( callback
  ) where

import           APIApto.AppMonad               ( AppIOM(unAppIOM)
                                                , IntAPISettings(..)
                                                , PinDecrypter
                                                , SSNDecrypter
                                                )
import           APIApto.Apto.Client            ( RequesterWithID )
import           APIApto.Card.Activate         as AC
                                                ( ActivationErrors(..)
                                                , activateCard
                                                )
import           APIApto.Card.ChangePin        as CP
                                                ( ChangePinErrors(..)
                                                , changeCardPin
                                                )
import           APIApto.Card.Close             ( closeCard )
import           APIApto.Card.Create            ( createCard )
import           APIApto.Card.Query            as QC
                                                ( GetLastFourErrors(..)
                                                , getCardLastFour
                                                )
import           APIApto.Cardholder.Create      ( createCardholder )
import           APIApto.Cardholder.Sync        ( syncUserFromApto )
import           APIApto.Cardholder.Update      ( updateCardholder )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Servant.Client                 ( ClientEnv )
import           Shared.Amqp                    ( AMQPCallback
                                                , CommandMessage(..)
                                                , EventMessage(..)
                                                , MessageBody(..)
                                                , ReplyMessage(ReplySuccessV1)
                                                , ReplySuccess(..)
                                                , TgthrMessage(..)
                                                , failureWithText
                                                , genSuccess
                                                )
import           Shared.Models.Apto.Card        ( AptoCard(..)
                                                , CardLastFour
                                                )
import           Shared.Models.Card             ( CardStatus(CardCreated) )
import           Shared.Models.Cardholder       ( CardholderId
                                                  ( AptoPaymentsCH
                                                  , PayWithPrivacyCH
                                                  )
                                                )
import           Shared.Models.User             ( UserState(..)
                                                , changesRequireReKYC
                                                )
import           Shared.TgthrMessages.Accounts  ( AccountsEvent(..) )
import           Shared.TgthrMessages.Apto      ( AptoCmd(..)
                                                , AptoEvent(..)
                                                , AptoReplies(GetLastFourReply)
                                                )
import           Shared.WebAPI.ApiApto.API      ( CreateCardholderAction(..) )
import           Shared.WebAPI.General.API      ( midToTrace )


callback
  :: SSNDecrypter
  -> PinDecrypter
  -> RequesterWithID
  -> ClientEnv
  -> AMQPCallback
-- | Create a cardholder with Apto
-- | @migrated
callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = EventV1 (AccountsEvt UserStateChange { useStatus = UserWaitingOnKYC, ..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ EventV1 AccountsEvt UserStateChange UserWaitingOnKYC "
      >> print (useUser, mid)

    let fn = createCardholder trace useUser
    resEither <-
      (Right <$> runReaderT
          (unAppIOM fn)
          (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)
        )
        `catch` (return . Left)

    putStr "AccountsEvt UserStateChange UserWaitingOnKYC "
      >> print (useUser, mid)

    case resEither of
      Right res -> do
        let evts = case res of
              (CardholderCreated (AptoPaymentsCH cardholderID)) ->
                [AptoEvt $ AptoCardholderCreated useUser cardholderID]
              (CardholderCreated (PayWithPrivacyCH _)) -> []
              (CardholderUpdated _                   ) -> []
              CardholderNotUpdated                     -> []
        return (Nothing, evts)
      Left (e :: SomeException) -> do
        putStr "Errpr: AccountsEvt UserStateChange UserWaitingOnKYC failed "
          >> print (useUser, mid, e)
        return (Nothing, [])

-- | @migrated
callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = EventV1 (AccountsEvt UserStateChange { useStatus = state@(UserClosed _), ..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ EventV1 AccountsEvt UserStateChange UserClosed"
      >> print (useUser, state, mid)

    let fn = closeCard trace useUser
    runReaderT (unAppIOM fn)
               (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)

    return (Nothing, [])

-- | @migrated
callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = EventV1 (AccountsEvt UserWasUpdated {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ EventV1 AccountsEvt UserWasUpdated"
      >> print (uueUser, uueChanges, mid)

    events <- if changesRequireReKYC uueState uueChanges
      then do
        putStr "UserWasUpdated with createCardholder"
          >> print (uueUser, uueState, mid)

        let fn = updateCardholder trace uueUser

        res <- runReaderT
          (unAppIOM fn)
          (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)

        return $ case res of
          (CardholderCreated (AptoPaymentsCH cardholderID)) ->
            [AptoEvt $ AptoCardholderCreated uueUser cardholderID]
          (CardholderCreated (PayWithPrivacyCH _)) -> []
          (CardholderUpdated _                   ) -> []
          CardholderNotUpdated                     -> []
      else return []

    return (Nothing, events)

-- | Create cards @ Apto
-- | @migrated
callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = EventV1 (AccountsEvt UserStateChange { useStatus = UserActive, ..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ EventV1 AccountsEvt UserStateChange UserActive "
      >> print (useUser, mid)

    let fn = createCard trace useUser
    cards <- runReaderT
      (unAppIOM fn)
      (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)

    putStr "AccountsEvt UserStateChange UserActive done "
      >> print (useUser, mid)

    return (Nothing, fmap createEvent cards)
  where createEvent (u, c, d) = AptoEvt $ CardStateChanged u c CardCreated d

callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = CommandV1 (AptoCmd ActivateCard {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ CommandV1 AptoCmd ActivateCard " >> print (accUser, mid)

    let fn = activateCard trace accUser accLastFour
    res :: Either ActivationErrors AptoCard <-
      (Right <$> runReaderT
          (unAppIOM fn)
          (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)
        )
        `catch` (return . Left)

    putStr "AptoCmd ActivateCard done " >> print (accUser, mid, res)

    return $ case res of
      Right card ->
        ( Just genSuccess
        , [ AptoEvt $ CardStateChanged accUser
                                       (acdxId card)
                                       (acdxStatus card)
                                       (acdxDesign card)
          ]
        )
      Left AC.UserHasNoCard ->
        (Just $ failureWithText "Can't find card for user", [])
      Left LastFourNotCorrect ->
        (Just $ failureWithText "LastFourIncorrect", [])
      Left (AC.HttpFailure _) -> (Just $ failureWithText "API error", [])

callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = CommandV1 (AptoCmd ChangeCardPin {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ CommandV1 AptoCmd ChangeCardPin " >> print (cccUser, mid)

    let fn = changeCardPin trace cccUser cccPinEnc
    res <-
      (Right <$> runReaderT
          (unAppIOM fn)
          (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)
        )
        `catch` (return . Left)

    putStr "AptoCmd ChangeCardPin done " >> print (cccUser, mid, res)

    return $ case res of
      Right _ -> (Just genSuccess, [])
      Left CardHasIncorrectState ->
        (Just $ failureWithText "Card state not correct", [])
      Left CP.UserHasNoCard -> (Just $ failureWithText "Card is missing", [])
      Left CP.HTTPFailure   -> (Just $ failureWithText "API Error", [])

callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = CommandV1 (AptoCmd GetLastFour {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ CommandV1 AptoCmd GetLastFour " >> print (lfcUser, mid)

    let fn = getCardLastFour trace lfcUser
    res :: Either GetLastFourErrors CardLastFour <-
      (Right <$> runReaderT
          (unAppIOM fn)
          (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)
        )
        `catch` (return . Left)

    putStr "AptoCmd GetLastFour done " >> print (lfcUser, mid, res)

    return $ case res of
      Right lastFour ->
        ( Just . ReplySuccessV1 . AptoReplySuccess $ GetLastFourReply lfcUser
                                                                      lastFour
        , []
        )
      Left QC.UserDoesntHaveCard ->
        (Just $ failureWithText "Can't find card for user", [])
      Left QC.HttpFailure -> (Just $ failureWithText "API error", [])

callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = CommandV1 (AptoCmd CreateCardholder {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ CommandV1 AptoCmd CreateCardholder "
      >> print (cccUser, mid)

    let fn = createCardholder trace cccUser
    res <- runReaderT
      (unAppIOM fn)
      (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)


    putStr "AptoCmd CreateCardholder done" >> print (cccUser, mid)

    let evts = case res of
          (CardholderCreated (AptoPaymentsCH cardholderID)) ->
            [AptoEvt $ AptoCardholderCreated cccUser cardholderID]
          (CardholderCreated (PayWithPrivacyCH _)) -> []
          (CardholderUpdated _                   ) -> []
          CardholderNotUpdated                     -> []
    return (Just genSuccess, evts)

callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = CommandV1 (AptoCmd CloseCurrentCard {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ CommandV1 AptoCmd CloseCurrentCard "
      >> print (xccUser, mid)

    let fn = closeCard trace xccUser
    runReaderT (unAppIOM fn)
               (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)

    putStr "AptoCmd CloseCurrentCard done " >> print (xccUser, mid)

    return (Just genSuccess, [])

callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = CommandV1 (AptoCmd CreateCard {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ CommandV1 AptoCmd CreateCard " >> print (cxcUser, mid)

    let fn = createCard trace cxcUser
    cards <- runReaderT
      (unAppIOM fn)
      (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)

    putStr "AptoCmd CreateCard done " >> print (cxcUser, mid)

    return (Just genSuccess, fmap createEvent cards)
  where createEvent (u, c, d) = AptoEvt $ CardStateChanged u c CardCreated d

callback ssnDecrypt pinDecrypt client accountsEnv _pub mid TgthrMessage { tgthrBody = CommandV1 (AptoCmd SyncUserWithApto {..}) }
  = do
    trace <- midToTrace mid
    putStr "RabbitMQ CommandV1 AptoCmd SyncUserWithApto "
      >> print (sacUser, mid)

    let fn = syncUserFromApto trace sacUser
    res <- runReaderT
      (unAppIOM fn)
      (IntAPISettings client ssnDecrypt pinDecrypt accountsEnv)

    putStr "AptoCmd SyncUserWithApto done " >> print (sacUser, mid)

    return $ case res of
      Nothing -> (Just genSuccess, [])
      Just kycStatus ->
        (Just genSuccess, [AptoEvt $ AptoKYCUpdated sacUser kycStatus])

-- Empty
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = EventV1 (MailerEvt _) } =
  return (Nothing, [])
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = EventV1 (AccountsEvt _) } =
  return (Nothing, [])
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = EventV1 (PayEvt _) } =
  return (Nothing, [])
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = EventV1 (AptoEvt _) } =
  return (Nothing, [])
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = EventV1 (AnalyticsEvt _) } =
  return (Nothing, [])
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = EventV1 (ExceptionEvt _) } =
  return (Nothing, [])
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = CommandV1 _ } =
  return (Nothing, [])
callback _ _ _ _ _ _ TgthrMessage { tgthrBody = ReplyV1 _ } =
  return (Nothing, [])
