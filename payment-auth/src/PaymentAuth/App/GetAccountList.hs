module PaymentAuth.App.GetAccountList where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB
                                                  ( getAccessToken
                                                  , getPlaidAuth
                                                  )
                                                )
import           PaymentAuth.Plaid              ( extractChecking )
import           Shared.Models.User             ( UserID )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AccountDetails )

type BankAccountName = Text

data GetTheAccountListErrors = NoAccessToken
  deriving (Eq, Show)

instance Exception GetTheAccountListErrors

getTheAccountList
  :: (HasPlaidDB m, MonadIO m) => UserID -> m (Either Text [AccountDetails])
getTheAccountList uid = do
  liftIO $ putStrLn ("start getTheAccountList for " <> show uid)
  accToken <- getAccessToken uid

  liftIO $ putStrLn ("getPlaidAuth for " <> show uid)
  accounts <- case accToken of
    Nothing  -> throw NoAccessToken
    Just tok -> getPlaidAuth tok

  liftIO $ putStr "(uid, accounts)" >> print (uid, accounts)
  let normalixedAccts = extractChecking <$> accounts

  liftIO $ putStr "normalixedAccts" >> print (uid, normalixedAccts)

  return normalixedAccts
