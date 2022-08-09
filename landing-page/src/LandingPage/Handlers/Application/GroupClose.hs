{-|
Module      : Group List handler
Description : Get a list of all groups for a user
Maintainer  : adam@example.com
Stability   : experimental
-}
module LandingPage.Handlers.Application.GroupClose where

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
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupMember(mbrUser)
                                                , GroupModel(grpMembers)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( CloseGroupBody(CloseGroupBody)
                                                , Routes(..)
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

closeGroup :: V.Key SessionData -> ClientEnv -> ActionM ()
closeGroup sKey accountsEnv = do
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

  let memberList = mbrUser <$> grpMembers groupModel
  let isMember   = uid `elem` memberList

  if not isMember
    then
      status status403
      >> Scotty.json (object [("error", "no access")])
      >> finish
    else do
      let closeGroupFn =
            _GroupClose asClientM trace groupId $ CloseGroupBody uid
      res <- liftAndCatchIO $ runClientM closeGroupFn accountsEnv
      case res of
        Right _ -> Scotty.json (object [])
        Left  e -> do
          liftAndCatchIO $ putStr "Error: closeGroup " >> print
            (e, uid, groupId)
          status status500 >> Scotty.json (object [("error", "500")])
