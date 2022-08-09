{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Group List handler
Description : Get a list of all groups for a user
Maintainer  : adam@example.com
Stability   : experimental
-}
module LandingPage.Handlers.Application.GroupList where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( nub )
import           Data.UUID                      ( nil )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types             ( status500 )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Group            ( GroupMember(mbrUser)
                                                , GroupModel(..)
                                                , allGroupStates
                                                )
import           Shared.Models.User             ( UserID(..)
                                                , UserModel(..)
                                                )
import           Shared.Utils                   ( fromRight )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , asClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                , status
                                                )

getGroupList :: V.Key SessionData -> ClientEnv -> ActionM ()
getGroupList sKey accountsEnv = do
  uid   <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  let getGroups = _GroupsForUser asClientM trace uid allGroupStates
  let getUser u = runClientM (_UserGet asClientM trace u) accountsEnv

  groupsE <- liftAndCatchIO $ runClientM getGroups accountsEnv
  case groupsE of
    Left  e      -> status status500 >> Scotty.json (object ["error" .= show e])
    Right groups -> do
      let groupsWithoutNil = filter
            (\GroupModel {..} -> UserID nil `notElem` fmap mbrUser grpMembers)
            groups
      let members = concatMap grpMembers groupsWithoutNil
      let users   = nub $ fmap mbrUser members
      userModelsE <- liftAndCatchIO $ mapM getUser users
      let userModels  = fmap fromRight userModelsE
          publicUsers = fmap
            (\UserModel {..} -> object
              [ "id" .= usrUserID
              , "fname" .= usrFirstName
              , "lname" .= usrLastName
              , "email" .= usrEmail
              ]
            )
            userModels
      Scotty.json $ object
        ["groups" .= groups, "users" .= publicUsers, "version" .= (1 :: Int)]
