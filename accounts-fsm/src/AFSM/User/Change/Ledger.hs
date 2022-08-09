module AFSM.User.Change.Ledger where

import           AFSM.FSM.User                  ( UserEvent )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(getUserById) )
import           AFSM.Monad.HasPaymentAuthClient
                                                ( HasPaymentAuthClient(..)
                                                , TraceContext
                                                )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , SomeException
                                                )
import           Control.Monad.Reader           ( MonadIO )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Ids              ( UserID(UserID) )
import           Shared.Models.Ledger.Journal   ( JournalType(..)
                                                , PaymentMethod(..)
                                                , journalOwner
                                                , searchFor
                                                )
import           Shared.Models.User             ( UserModel
                                                  ( usrFirstName
                                                  , usrLastName
                                                  )
                                                )

createPayTgthrLedger
  :: (HasPaymentAuthClient m, HasGetUserDB m, MonadIO m, MonadCatch m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
createPayTgthrLedger trace uid = do
  tracePrint trace "createPayTgthrLedger for " uid

  let jType = PayTgthr uid

  createJournal trace jType

createFundingSourceLedger
  :: (HasPaymentAuthClient m, HasGetUserDB m, MonadIO m, MonadCatch m)
  => TraceContext
  -> UserID
  -> Text
  -> m [UserEvent]
createFundingSourceLedger trace uid fsId = do
  tracePrint trace "createFundingSourceLedger for " (uid, fsId)

  let jType = FundingSource uid $ DwollaACH fsId

  createJournal trace jType

createJournal
  :: (HasPaymentAuthClient m, HasGetUserDB m, MonadIO m, MonadCatch m)
  => TraceContext
  -> JournalType
  -> m [UserEvent]
createJournal trace jType = do
  let uid = UserID $ fromMaybe (error "Error: no UID") $ journalOwner jType

  userMaybe <- getUserById uid
  let user = fromMaybe (error $ "Error: User missing " <> show uid) userMaybe

  let usersName = case usrFirstName user <> Just " " <> usrLastName user of
        Nothing  -> "User"
        Just " " -> "User"
        Just t   -> t

  let search = case jType of
        ExternalAccount _ -> error "Error: not a valid query"
        VirtualAccount    -> error "Error: not a valid query"
        jt                -> searchFor jt

  getRes <-
    getLedger trace search
      `catch` (\(e :: SomeException) -> do
                traceError trace
                           "Error: createJournal could not get ledger"
                           (show e, jType)
                return []
              )

  case getRes of
    [] -> do
      let ledgerName = usersName <> "'s " <> T.pack (show jType)
      tracePrint trace "Creating journal, exists" (jType, ledgerName)
      createLedger trace jType ledgerName
        `catch` (\(e :: SomeException) -> do
                  traceError trace
                             "Error: createJournal could not create ledger"
                             (show e, jType)
                  return ()
                )
    (j : _) -> tracePrint trace "Not creating journal, exists" (jType, j)

  return []
