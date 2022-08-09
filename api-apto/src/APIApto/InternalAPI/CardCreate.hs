{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIApto.InternalAPI.CardCreate where

import qualified APIApto.Card.Create           as CC
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Servant                        ( ServerError
                                                , err500
                                                )
import           Shared.Models.Card             ( CardStatus(CardCreated)
                                                , IssuerPlatform(AptoPayments)
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..) )
import           Shared.WebAPI.General.Issuer   ( CardCreatedResponse(..) )

createNewCard
  :: (HasAccounts m, HasAptoClient m, MonadIO m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> m CardCreatedResponse
createNewCard trace userId = do
  cardsCreated <- CC.createCard trace userId
  case cardsCreated of
    [] -> do
      liftIO $ putStr "Error: createNewCard created 0 cards " >> print
        (trace, userId)
      throwError err500
    [(_, cardId, cardDesign)] -> return $ CardCreatedResponse
      (AptoPayments cardId)
      cardDesign
      Nothing
      CardCreated
      "0000"
    (_, cardId, cardDesign) : _ -> do
      liftIO $ putStr "Error: createNewCard created more than 1 card " >> print
        (trace, userId, cardsCreated)
      return $ CardCreatedResponse (AptoPayments cardId)
                                   cardDesign
                                   Nothing
                                   CardCreated
                                   "0000"
