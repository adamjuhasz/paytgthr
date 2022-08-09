{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module APIDwolla.AppMonad where

import qualified APIDwolla.Dwolla.Client       as Dwolla
import           APIDwolla.Dwolla.Client        ( TokenMaker )
import           APIDwolla.Monad.Accounts       ( HasAccounts(..) )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient(..) )
import           APIDwolla.Monad.Payment        ( HasPayments(..) )
import           Control.Concurrent             ( MVar )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , asks
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Servant                        ( Handler
                                                , NoContent(NoContent)
                                                , ServerError
                                                )
import           Servant.Client                 ( ClientEnv )
import           Shared.Models.User             ( UserModel )
import qualified Shared.WebAPI.AccountsFSM.API as Accounts
import           Shared.WebAPI.AccountsFSM.Client
                                                ( UpdateUserBody(..)
                                                , accountsRoutes
                                                )
import           Shared.WebAPI.General.API      ( incrementTrace )
import qualified Shared.WebAPI.PaymentAuth.API as PayAuth
import           Shared.WebAPI.PaymentAuth.Client
                                                ( UpdatePaymentBody(..)
                                                , paymentauthRoutes
                                                )

data InternalAPISettings = InternalAPISettings
  { tokenMaker     :: IO TokenMaker
  , accountsEnv    :: ClientEnv
  , paymentsEnv    :: ClientEnv
  , isShuttingDown :: MVar Bool
  }

newtype AppWebM a =
    AppWebM { unAppWebM :: ReaderT InternalAPISettings Handler a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader InternalAPISettings
      , MonadError ServerError
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

instance HasDwollaClient AppWebM where
  createCustomer trace user = do
    token <- asks tokenMaker
    liftIO $ token >>= Dwolla.createCustomer trace user
  addBankAccount trace user = do
    token <- asks tokenMaker
    liftIO $ token >>= Dwolla.addBank trace user
  removeBankAccount trace sourceId = do
    token <- asks tokenMaker
    liftIO $ token >>= Dwolla.removeFundingSource trace sourceId
  createACHTransfer trace payment user = do
    token <- asks tokenMaker
    liftIO $ token >>= Dwolla.createTransfer trace payment user
  cancelACHTransfer trace payment = do
    token <- asks tokenMaker
    liftIO $ token >>= Dwolla.cancelTransfer trace payment

instance HasAccounts AppWebM where
  getUser trace userId = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = Accounts._UserGet $ accountsRoutes env
    let runClinet :: IO (Maybe UserModel) =
          (Just <$> fn trace' userId)
            `catch` (\(_ :: SomeException) -> return Nothing)
    liftIO runClinet
  updateUser trace userId changes = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = Accounts._UserUpdate $ accountsRoutes env
    NoContent <- liftIO $ fn trace' userId $ UpdateUserBody changes
    return ()

instance HasPayments AppWebM  where
  getPayment trace payId = do
    env    <- asks paymentsEnv
    trace' <- incrementTrace trace
    let fn = PayAuth._GetPayment $ paymentauthRoutes env
    let runClinet =
          (Just <$> fn trace' payId)
            `catch` (\(_ :: SomeException) -> return Nothing)
    liftIO runClinet
  updatePayment trace _ pid status ach methodId = do
    env    <- asks paymentsEnv
    trace' <- incrementTrace trace
    let fn = PayAuth._UpdatePayment $ paymentauthRoutes env
    NoContent <- liftIO $ fn trace' pid $ UpdatePaymentBody status ach methodId
    return ()
  makeVerificationPayment trace uid body = do
    env       <- asks paymentsEnv
    traceIncr <- incrementTrace trace
    let fn = PayAuth._MakeVerificationPayment $ paymentauthRoutes env
    NoContent <- liftIO $ fn traceIncr uid body
    return ()
