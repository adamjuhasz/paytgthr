{-# LANGUAGE StrictData #-}

{-|
Module      : Group List handler
Description : Get a list of all groups for a user
Maintainer  : adam@example.com
Stability   : experimental
-}
module LandingPage.Handlers.Application.GroupAccept where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
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
import           Network.HTTP.Types.Status      ( Status(Status, statusCode) )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , ClientError(..)
                                                , ResponseF(..)
                                                , runClientM
                                                )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupMember(mbrUser)
                                                , GroupModel(grpMembers)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , asClientM
                                                , incrementTrace
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

acceptGroup :: V.Key SessionData -> ClientEnv -> ActionM ()
acceptGroup sKey env = do
  uid <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  groupId :: GroupId <- GroupId . fromJust . fromText <$> param "id"

  let internalError e = do
        liftAndCatchIO $ putStr "Error: acceptGroup " >> print (uid, groupId, e)
        status status500 >> Scotty.json (object [("error", "500")]) >> finish

  trace <- createTrace

  groupE <- liftAndCatchIO $ runClientM (_GroupGet asClientM trace groupId) env
  groupModel <- case groupE of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } })
      -> status status404
        >> Scotty.json (object [("error", "group not found")])
        >> finish
    Left  e -> internalError e
    Right g -> return g

  let members  = mbrUser <$> grpMembers groupModel
  let isMember = uid `elem` members

  if not isMember
    then
      status status403
      >> Scotty.json (object [("error", "no access")])
      >> finish
    else do
      trace' <- incrementTrace trace
      res    <- liftAndCatchIO
        $ runClientM (_GroupApproveInvite asClientM trace' groupId uid) env
      case res of
        Left  e -> internalError e
        Right _ -> Scotty.json $ object ["group" .= groupId]
