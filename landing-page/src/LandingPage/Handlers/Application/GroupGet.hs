{-|
Module      : Get a specific group
Description : Get a list of all groups for a user
Maintainer  : adam@example.com
Stability   : experimental
-}
module LandingPage.Handlers.Application.GroupGet where

import           Control.Monad                  ( unless )
import           Data.Aeson                     ( object )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust )
import           Data.UUID                      ( fromText )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types             ( status403
                                                , status404
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupMember(..)
                                                , GroupModel(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , asClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , param
                                                , request
                                                , status
                                                )

getSpecificGroup :: V.Key SessionData -> ClientEnv -> ActionM ()
getSpecificGroup sKey accountsEnv = do
  uid <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  groupId :: GroupId <- GroupId . fromJust . fromText <$> param "id"
  trace <- createTrace

  let getGroup = _GroupGet asClientM trace groupId
  groupE     <- liftAndCatchIO $ runClientM getGroup accountsEnv
  groupModel <- case groupE of
    Left _ ->
      status status404
        >> Scotty.json (object [("error", "group not found")])
        >> finish
    Right g -> return g

  let members  = mbrUser <$> grpMembers groupModel
  let isMember = uid `elem` members

  unless
    isMember
    (status status403 >> Scotty.json (object [("error", "NotMember")]) >> finish
    )

  Scotty.json groupModel
