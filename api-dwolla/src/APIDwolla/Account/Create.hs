{-# LANGUAGE LambdaCase #-}

module APIDwolla.Account.Create where

import           APIDwolla.Monad.Accounts       ( HasAccounts(..) )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient(..) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Shared.Console
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                , UserTrait(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data CreateDwollaAccountErrors
  = HttpFailureCreateAccount
  | HttpFailureCreateBank [DwollaChanges]
  deriving (Show)
instance Exception CreateDwollaAccountErrors

type DwollaAccountId = Text
type DwollaFSId = Text

data DwollaChanges
  = DwollaAccountCreated DwollaAccountId
  | DwollaFSCreated DwollaFSId
  deriving (Show)

data CreateOptions
  = DontAddBank
  | AddBankIfNeeded
  deriving (Show)

createDwollaAccount
  :: (HasDwollaClient m, HasAccounts m, MonadIO m)
  => TraceContext
  -> UserID
  -> CreateOptions
  -> m [DwollaChanges]
createDwollaAccount trace userId options = do
  tracePrint trace "createDwollaAccount " (userId, options)

  model                       <- fromJust <$> getUser trace userId

  -- Do we need to create a new dwolla user?
  (dwollaId, customerChanges) <- case usrDwollaId model of
    Just uid -> return (uid, [])
    Nothing  -> do
      did <- createCustomer trace model
      case did of
        Left e -> do
          traceError trace
                     "Error: createDwollaAccount createCustomer "
                     (userId, e, model)
          throw HttpFailureCreateAccount
        Right customerId -> do
          let newDwollaId = decodeUtf8 customerId
          return (newDwollaId, [DwollaAccountCreated newDwollaId])

  -- don't always make a new funding source
  let newModel = model { usrDwollaId = Just dwollaId }
  bankChanges <-
    case (options, usrBankRouting newModel, usrBankAcount newModel) of
      (_              , Nothing, Nothing) -> return [] -- missing routing # & account #
      (_              , Nothing, Just _ ) -> return [] -- missing routing #
      (_              , Just _ , Nothing) -> return [] -- missing account #
      (DontAddBank    , Just _ , Just _ ) -> return [] -- will do in caller
      (AddBankIfNeeded, Just _ , Just _ ) -> do
        tracePrint trace "createDwollaAccount addBankAccount " userId
        fid <- addBankAccount trace newModel
        case fid of
          Left e -> do
            traceError trace
                       "Error: createDwollaAccount addBankAccount "
                       (userId, e, model)
            throw $ HttpFailureCreateBank customerChanges
          Right fundingId -> return
            (  []
            <> [ DwollaFSCreated fundingId
               | (usrDwollaFundingId model /= Just fundingId)
                 && (usrBankVerified model == Just True)
               ]
            <> [ DwollaFSCreated fundingId
               | (usrDwollaFundingId model /= Just fundingId)
                 && (usrBankVerified model == Just False)
               ]
            )

  let changes = customerChanges <> bankChanges
  tracePrint trace "createDwollaAccount (changes): " (userId, changes)

  let accountChanges = fmap
        (\case
          DwollaAccountCreated did  -> (DwollaCustomerId, Just did)
          DwollaFSCreated      fsid -> (DwollaFundingId, Just fsid)
        )
        changes

  -- @todo move outside
  unless (null changes) (updateUser trace userId accountChanges)

  return changes
