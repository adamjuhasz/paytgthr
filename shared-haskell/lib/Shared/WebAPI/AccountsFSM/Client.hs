{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE FlexibleContexts #-}


module Shared.WebAPI.AccountsFSM.Client
  ( module Shared.WebAPI.AccountsFSM.Client
  , module Shared.WebAPI.AccountsFSM.API
  , TraceContext(..)
  , randomTrace
  ) where

import           Control.Exception              ( SomeException
                                                , catch
                                                , throwIO
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Servant                        ( NoContent(..)
                                                , Proxy(..)
                                                )
import           Servant.API.Generic            ( ToServantApi
                                                , genericApi
                                                )
import           Servant.Client                 ( ClientEnv
                                                , ClientM
                                                , runClientM
                                                )
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                , genericClientHoist
                                                )
import           Shared.Models.Group            ( GroupModel
                                                , GroupStatus(GroupActive)
                                                )
import           Shared.Models.Payment          ( PaymentFailureCode
                                                , PaymentMethod
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                )
import           Shared.WebAPI.AccountsFSM.API
import           Shared.WebAPI.General.API      ( randomTrace )

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

accountsRoutes :: ClientEnv -> Routes (AsClientT IO)
accountsRoutes env =
  genericClientHoist (\x -> runClientM x env >>= either throwIO return)

accountsClientM :: Routes (AsClientT ClientM)
accountsClientM = asClientM

asClientM :: Routes (AsClientT ClientM)
asClientM = genericClient

getUserTemplate
  :: (MonadIO m) => TraceContext -> UserID -> ClientEnv -> m (Maybe UserModel)
getUserTemplate trace aUserID env = do
  trace' <- incrementTrace trace
  let fn = _UserGet $ accountsRoutes env
  let runClinet :: IO (Maybe UserModel) =
        (Just <$> fn trace' aUserID)
          `catch` (\(e :: SomeException) -> do
                    putStr "Error: _UserGet error " >> print (aUserID, e, trace)
                    return Nothing
                  )
  liftIO runClinet

getGroupForUserTemplate
  :: (MonadIO m) => TraceContext -> UserID -> ClientEnv -> m (Maybe GroupModel)
getGroupForUserTemplate trace aUserID env = do
  trace' <- incrementTrace trace
  let fn = _GroupsForUser $ accountsRoutes env
  let runClinet =
        (Just <$> fn trace' aUserID [GroupActive])
          `catch` (\(e :: SomeException) -> do
                    putStr "Error: _GroupsForUser error "
                      >> print (aUserID, e, trace)
                    return Nothing
                  )
  res <- liftIO runClinet
  case res of
    Nothing -> do
      liftIO $ putStr "Error: getting groups " >> print (trace, aUserID)
      error "Error: Could not get groups due to request error"
    Just []          -> return Nothing
    Just [group    ] -> return $ Just group
    Just (group : _) -> return $ Just group

setUsersKYCStateTemplate
  :: (MonadIO m)
  => TraceContext
  -> UserID
  -> ChangeKYCStateBody
  -> ClientEnv
  -> m NoContent
setUsersKYCStateTemplate trace aUserID kycState env = do
  let fn = _UserSetKYCState $ accountsRoutes env
  liftIO $ fn trace aUserID kycState

removeFundingSourceTemplate
  :: (MonadIO m)
  => TraceContext
  -> UserID
  -> PaymentMethod
  -> Maybe PaymentFailureCode
  -> ClientEnv
  -> m ()
removeFundingSourceTemplate trace uid method failCode env = do
  trace' <- incrementTrace trace
  let fn   = _UserRemoveFS $ accountsRoutes env
  let body = RemoveBankFSBody method failCode
  NoContent <- liftIO $ fn trace' uid body
  return ()

getAllActiveUsersTemplate
  :: (MonadIO m) => TraceContext -> ClientEnv -> m [UserID]
getAllActiveUsersTemplate trace env = do
  trace' <- incrementTrace trace
  let fn = _UsersQueryAllActive $ accountsRoutes env
  liftIO $ fn trace'
