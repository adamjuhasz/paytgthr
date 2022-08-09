{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Redundant variable capture" -}

module APIDwolla.FundingSource.Create where

import           APIDwolla.Account.Create       ( CreateOptions(DontAddBank)
                                                , DwollaChanges(..)
                                                , createDwollaAccount
                                                )
import           APIDwolla.Monad.Accounts       ( HasAccounts(..) )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient(..) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( forM )
import           Control.Monad.Reader           ( MonadIO )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Text                      ( Text )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.User             ( RedactedText(..)
                                                , UserID
                                                , UserModel(..)
                                                , UserTrait(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data CreateBankAccountErrors
  = SameAccount
  | HTTPFailure
  deriving (Show)
instance Exception CreateBankAccountErrors

type RoutingNum = Text
type AccountNum = Text
type AccountName = Text
type DwollaFsId = Text

createBankAccount
  :: (HasAccounts m, HasDwollaClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> RoutingNum
  -> AccountNum
  -> AccountName
  -> m (DwollaFsId, [DwollaChanges])
createBankAccount trace user abaRouting ddaNumber accountName = do
  model                             <- fromJust <$> getUser trace user

  -- if we don't have a dwolla account, make one
  (accountChanges, modelWithDwolla) <- if isNothing (usrDwollaId model)
    then do
      tracePrint trace
                 "createBankAccount createDwollaAccount "
                 (user, usrRevision model)
      evts         <- createDwollaAccount trace user DontAddBank
      updatedModel <- fromJust <$> getUser trace user
      return (evts, updatedModel)
    else return ([], model)

  -- if we have a current bank account, remove it
  removeEvents <- forM
    (usrDwollaFundingId model)
    (\fsId -> do
      tracePrint trace
                 "createBankAccount removeBankAccount "
                 (user, usrRevision model, fsId)
      res <- removeBankAccount trace fsId
      case res of
        Left e -> tracePrint trace
                             "createBankAccount removeBankAccount "
                             (user, usrRevision model, e, fsId)
        Right _ -> return ()
      return []
    )

  let newModel = modelWithDwolla
        { usrDwollaFundingId = Nothing
        , usrBankRouting     = Just $ RedactedText abaRouting
        , usrBankAcount      = Just $ RedactedText ddaNumber
        , usrBankAccountName = Just accountName
        }

  let previosEvents = accountChanges <> fromMaybe [] removeEvents

  case getFSCreated accountChanges of
    Just fsid -> return (fsid, previosEvents)
    Nothing   -> do
      -- add this bank account
      tracePrint trace "createBankAccount addBankAccount " user
      fsid <- addBankAccount trace newModel >>= \case
        Left e -> do
          traceError trace
                     "Error: createBankAccount addBankAccount failed: "
                     (user, usrRevision model, e, newModel)
          throw HTTPFailure
        Right fsid -> return fsid

      -- update user model
      let changes = [(DwollaFundingId, Just fsid)]
      tracePrint trace "createBankAccount (changes): " (user, changes)
      updateUser trace user changes

      return (fsid, previosEvents <> [DwollaFSCreated fsid])

getFSCreated :: [DwollaChanges] -> Maybe DwollaFsId
getFSCreated [] = Nothing
getFSCreated (DwollaFSCreated fsid : _) = Just fsid
getFSCreated (DwollaAccountCreated _ : changes) = getFSCreated changes
