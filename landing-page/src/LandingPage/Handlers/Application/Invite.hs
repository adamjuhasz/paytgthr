{-# LANGUAGE RecordWildCards #-}

module LandingPage.Handlers.Application.Invite where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types             ( Status(Status, statusCode)
                                                , status404
                                                , status409
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , ClientError(FailureResponse)
                                                , ResponseF
                                                  ( Response
                                                  , responseStatusCode
                                                  )
                                                , runClientM
                                                )
import           Shared.Models.Invite           ( PartnerInvite(..) )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( AcceptInviteResponse
                                                  ( AcceptInviteResponse
                                                  , newGroupId
                                                  )
                                                , Routes
                                                  ( _UserAcceptInvite
                                                  , _UserMakeInvite
                                                  )
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

getInviteCode :: V.Key SessionData -> ClientEnv -> ActionM ()
getInviteCode sKey accountsEnv = do
  uid   <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  let getCode = _UserMakeInvite asClientM trace uid
  codeEi <- liftAndCatchIO $ runClientM getCode accountsEnv
  case codeEi of
    Left e -> do
      liftAndCatchIO $ putStr "Error: getInviteCode _UserMakeInvite " >> print
        (uid, e)
      status status500 >> Scotty.json (object []) >> finish
    Right PartnerInvite {..} ->
      Scotty.json (object ["code" .= inviteCode, "created" .= inviteCreated])

acceptInvite :: V.Key SessionData -> ClientEnv -> ActionM ()
acceptInvite sKey accountsEnv = do
  uid   <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  code  <- param "code"

  let sendCode = _UserAcceptInvite asClientM trace uid code
  codeEi <- liftAndCatchIO $ runClientM sendCode accountsEnv
  case codeEi of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } })
      -> status status404 >> Scotty.json (object []) >> finish
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 409 } })
      -> status status409 >> Scotty.json (object []) >> finish
    Left e -> do
      liftAndCatchIO $ putStr "Error: acceptInvite _UserAcceptInvite " >> print
        (uid, code, e)
      status status500 >> Scotty.json (object []) >> finish
    Right AcceptInviteResponse {..} ->
      Scotty.json (object ["groupid" .= newGroupId])

  return ()
