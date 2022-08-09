{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore "Reduce duplication" -}

module APIApto.AppMonad where

import qualified APIApto.Apto.Actions          as Actions
import           APIApto.Apto.Client            ( RequesterWithID )
import           APIApto.Monad.Accounts         ( HasAccounts(..) )
import           APIApto.Monad.Apto             ( HasAptoClient(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Servant                        ( Handler
                                                , NoContent(..)
                                                , ServerError
                                                )
import           Servant.Client                 ( ClientEnv )
import           Shared.Vault                  as V
                                                ( CipherText
                                                , PlainText
                                                )
import qualified Shared.WebAPI.AccountsFSM.API as Accounts
import           Shared.WebAPI.AccountsFSM.Client
                                                ( accountsRoutes
                                                , getGroupForUserTemplate
                                                , getUserTemplate
                                                )
import           Shared.WebAPI.General.API      ( incrementTrace )

data RunEnvironMent
  = Production
  | Staging
  | Development

type SSNDecrypter = (CipherText -> IO V.PlainText)
type PinDecrypter = (CipherText -> IO V.PlainText)

data IntAPISettings = IntAPISettings
  { aptoRequester :: RequesterWithID
  , ssnDecrypter  :: SSNDecrypter
  , pinDecrypter  :: PinDecrypter
  , accountsEnv   :: ClientEnv
  }

newtype AppIOM a =
    AppIOM { unAppIOM :: ReaderT IntAPISettings IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader IntAPISettings
      )

newtype AppWebM a =
    AppWebM { unAppWebM :: ReaderT IntAPISettings Handler a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader IntAPISettings
      , MonadError ServerError
      )

instance HasAptoClient AppIOM where
  activateCard mid cardID lastFour = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.activateCard (client stringMid) cardID lastFour
  changePin mid cardId pinEnc = do
    client    <- asks aptoRequester
    decrypter <- asks pinDecrypter
    let stringMid = show mid
    liftIO $ Actions.changePin decrypter (client stringMid) cardId pinEnc
  createCard mid users = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.createCard (client stringMid) users
  closeCard mid cardId = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.closeCard (client stringMid) cardId
  getCard mid cardId = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.getCard (client stringMid) cardId
  createCardholder mid user = do
    client    <- asks aptoRequester
    decrypter <- asks ssnDecrypter
    let stringMid = show mid
    liftIO $ Actions.createCardholder decrypter (client stringMid) mid user
  getCardholder mid cardholderId = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.getCardholder (client stringMid) cardholderId

instance HasAccounts AppIOM where
  getUser trace userId = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getUserTemplate trace' userId env
  getGroupForUser trace userId = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getGroupForUserTemplate trace' userId env
  setKycState _ _ _ = error "setKycState for AppIOM missing"

instance HasAptoClient AppWebM where
  activateCard mid cardID lastFour = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.activateCard (client stringMid) cardID lastFour
  changePin mid cardId pinEnc = do
    client    <- asks aptoRequester
    decrypter <- asks pinDecrypter
    let stringMid = show mid
    liftIO $ Actions.changePin decrypter (client stringMid) cardId pinEnc
  createCard mid users = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.createCard (client stringMid) users
  closeCard mid cardId = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.closeCard (client stringMid) cardId
  getCard mid cardId = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.getCard (client stringMid) cardId
  createCardholder mid user = do
    client    <- asks aptoRequester
    decrypter <- asks ssnDecrypter
    let stringMid = show mid
    liftIO $ Actions.createCardholder decrypter (client stringMid) mid user
  getCardholder mid cardholderId = do
    client <- asks aptoRequester
    let stringMid = show mid
    liftIO $ Actions.getCardholder (client stringMid) cardholderId

instance HasAccounts AppWebM where
  getUser trace userId = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getUserTemplate trace' userId env
  getGroupForUser trace userId = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getGroupForUserTemplate trace' userId env
  setKycState trace userId kycState = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = Accounts._UserSetKYCState $ accountsRoutes env
    NoContent <- liftIO $ fn trace' userId kycState
    return ()
