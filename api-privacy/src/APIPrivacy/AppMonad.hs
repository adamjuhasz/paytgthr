{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore "Reduce duplication" -}

module APIPrivacy.AppMonad where

import           APIPrivacy.Monad.HasAccounts   ( HasAccounts(..) )
import           APIPrivacy.Monad.HasAppSettings
import           APIPrivacy.Monad.HasClient     ( HasClient(..) )
import           APIPrivacy.Monad.HasDecryption ( HasDecryption(..) )
import           APIPrivacy.Monad.HasPrivacyClient
                                                ( HasPrivacyClient(..) )
import           APIPrivacy.PrivacyClient       ( generatePrivAuth
                                                , runClientM
                                                )
import           Control.Concurrent             ( MVar
                                                , tryReadMVar
                                                )
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(ReaderT) )
import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Servant                        ( Handler
                                                , ServerError
                                                )
import           Servant.Client                 ( ClientEnv(baseUrl) )
import           Shared.Vault                  as V
                                                ( CipherText
                                                , PlainText
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , accountsRoutes
                                                , getUserTemplate
                                                , incrementTrace
                                                )

data RunEnvironMent
  = Production
  | Staging
  | Development

type SSNDecrypter = (CipherText -> IO V.PlainText)
type PinDecrypter = (CipherText -> IO V.PlainText)

data APISettings = APISettings
  { ssnDecrypter   :: SSNDecrypter
  , pinDecrypter   :: PinDecrypter
  , accountsEnv    :: ClientEnv
  , payAuthEnv     :: ClientEnv
  , privacyEnv     :: ClientEnv
  , privacyKey     :: Text
  , isShuttingDown :: MVar Bool
  }

newtype AppWebM a =
    AppWebM { unAppWebM :: ReaderT APISettings Handler a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader APISettings
      , MonadError ServerError
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

instance HasAppSettings AppWebM where
  getIsShuttingDown = do
    mvar <- asks isShuttingDown
    liftIO $ tryReadMVar mvar

instance HasPrivacyClient AppWebM where
  getPrivacyEnv     = asks privacyEnv
  getPrivacyAPIKey  = asks privacyKey
  getPrivacyAuth    = asks (generatePrivAuth . privacyKey)
  getPrivacyURLBase = asks
    ( T.replace "\"" ""
    . TE.decodeUtf8
    . BSL.toStrict
    . encode
    . baseUrl
    . privacyEnv
    )
  runPrivacyCmd monad = do
    env <- asks privacyEnv
    liftIO $ runClientM monad env

instance HasAccounts AppWebM where
  getUser trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    getUserTemplate trace' uid env
  getCardsFor trace uid = do
    env    <- asks accountsEnv
    trace' <- incrementTrace trace
    let fn = _UserCardList $ accountsRoutes env
    liftIO $ fn trace' uid

instance HasDecryption AppWebM where
  decryptSSN _trace encSSN = do
    decrypter <- asks ssnDecrypter
    liftIO $ decrypter encSSN
  decryptPIN _trace encPIN = do
    decrypter <- asks pinDecrypter
    liftIO $ decrypter encPIN

instance HasClient AppWebM where
  accountsClient fn = do
    env <- asks accountsEnv
    liftIO $ runClientM fn env
  payAuthClient fn = do
    env <- asks payAuthEnv
    liftIO $ runClientM fn env
