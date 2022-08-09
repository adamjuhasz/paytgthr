{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.Card.SetPin where

import           AFSM.Monad.HasEventTracking    ( HasEventTracking(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasIssuerClient    as IC
                                                ( HasIssuerClient(..) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO )
import           Servant                        ( NoContent(..)
                                                , ServerError(errBody)
                                                , err500
                                                )
import           Shared.Console                 ( traceError )
import           Shared.Models.Ids              ( CardId
                                                , UserID
                                                )
import           Shared.WebAPI.AccountsFSM.API as Accounts
                                                ( ChangePinBody(..)
                                                , TraceContext
                                                )
import           Shared.WebAPI.ApiPrivacy.API  as Privacy
                                                ( ChangePinBody(..) )

setCardPin
  :: ( HasIssuerClient m
     , HasEventTracking m
     , HasGetUserDB m
     , MonadIO m
     , MonadError ServerError m
     )
  => TraceContext
  -> UserID
  -> CardId
  -> Accounts.ChangePinBody
  -> m NoContent
setCardPin trace uid cid Accounts.ChangePinBody {..} = do
  cardMaybe <- getCard trace cid
  card      <- case cardMaybe of
    Nothing -> do
      traceError trace "Error: card for ActivateCardBody not found " (uid, cid)
      throwError err500 { errBody = "card not found" }
    Just cm -> return cm

  IC.setCardPin trace card $ Privacy.ChangePinBody newPinText newPinEncrypted
  trackEvent uid "DebitCard Pin Changed"

  return NoContent
