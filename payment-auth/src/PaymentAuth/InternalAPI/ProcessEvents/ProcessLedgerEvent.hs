{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.InternalAPI.ProcessEvents.ProcessLedgerEvent where

import           PaymentAuth.Monad.EventTracking
                                                ( (.=)
                                                , HasEventTracking(..)
                                                , object
                                                )

import           Shared.Models.Currency         ( getMonetaryValue )
import           Shared.Models.Ledger.Common    ( factAmount
                                                , factType
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.WebAPI.General.API      ( TraceContext )

processLedgerEntry
  :: (HasEventTracking m) => TraceContext -> LedgerEntry -> m ()
processLedgerEntry _     LedgerEntry { lenUser = Nothing }     = return ()
processLedgerEntry trace LedgerEntry { lenUser = Just uid, ..} = do
  trackOneOffTrait
    uid
    ["ledgerBalance" .= (fromRational $ getMonetaryValue lenBalance :: Double)]

  trackEventWithProps
    trace
    uid
    "User Ledger Updated"
    (object
      [ "ledgerBalance"
        .= (fromRational $ getMonetaryValue lenBalance :: Double)
      , "ledgerChange"
        .= (fromRational $ getMonetaryValue $ factAmount lenFact :: Double)
      , "ledgerFact" .= factType lenFact
      , "ledgerRevision" .= lenRevision
      , "timeOf" .= lenCreatedAt
      ]
    )

  return ()


