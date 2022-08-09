module Scaffolding.Mocks where

import           PaymentAuth.Types              ( DBActions(..) )
import           Shared.Models.RiskScore        ( RiskFact(InitialRisk)
                                                , RiskScore
                                                  ( RiskScore
                                                  , rskChange
                                                  , rskCreatedAt
                                                  , rskFact
                                                  , rskMsgSource
                                                  , rskRev
                                                  , rskTrustScore
                                                  , rskUser
                                                  )
                                                )
import           Shared.Utils                   ( stringToTime )

mockDBActions :: DBActions
mockDBActions = DBActions
  { dbInsertBalance              = \_ _ _ -> return ()
  , dbGetTokens                  = return []
  , dbGetPrimaryAccount          = \_ -> return Nothing
  , dbGetAccessToken             = \_ -> return Nothing
  , dbGetPlaidTokenRow           = \_ -> return Nothing
  , dbInsertPlaidTokenRow        = \_ -> return ()
  , dbUpdateTokenPrimary         = \_ _ _ -> return ()
  , dbInsertToken                = \_ _ _ -> return ()
  , dbGetRecentBalanceSince      = \_ _ -> return Nothing
  , dbGetUserFromItem            = \_ -> return Nothing
  , dbSaveTransaction            = \_ -> return ()
  , dbLoadTransaction            = \_ -> return Nothing
  , dbLoadTransactionFromAptoId  = \_ -> return Nothing
  , dbGetPendingTransactions     = \_ -> return []
  , dbSavePayment                = \_ -> return ()
  , dbLoadPayment                = \_ -> return Nothing
  , dbGetPaymentFromSourceId     = \_ -> return Nothing
  , dbGetUsersPendingPayments    = \_ -> return []
  , dbGetPendingPaymentCreatedAt = \_ -> return Nothing
  , dbSaveLedgerEntry            = \_ -> return ()
  , dbGetUsersWithBalances       = return []
  , dbGetAllPendingTransactions  = return []
  , dbGetUsersRisk               = \mid uid -> return RiskScore
    { rskUser       = uid
    , rskRev        = 1
    , rskTrustScore = 100
    , rskChange     = 0
    , rskFact       = InitialRisk
    , rskMsgSource  = mid
    , rskCreatedAt  = stringToTime "2019-10-04T20:15:38.719+00:00"
    }
  , dbSaveRiskScore              = \_ -> return ()
  , dbGetUsersTransactions       = \_ _ -> return []
  }
