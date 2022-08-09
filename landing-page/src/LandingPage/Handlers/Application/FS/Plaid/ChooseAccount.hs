module LandingPage.Handlers.Application.FS.Plaid.ChooseAccount where

import           Data.Aeson                     ( object )
import           Data.Functor                   ( (<&>) )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types.Status      ( Status(Status, statusCode)
                                                , status403
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
import           Shared.WebAPI.PaymentAuth.Client
                                                ( Routes(..)
                                                , payAuthClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , request
                                                , status
                                                )

chooseAccountWithPlaid :: V.Key SessionData -> ClientEnv -> ActionM ()
chooseAccountWithPlaid sKey paymentEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  accountInfo <- Scotty.jsonData
  trace <- createTrace

  let setAccount = _SetPlaidAccount payAuthClientM trace user accountInfo
  res <- liftAndCatchIO $ runClientM setAccount paymentEnv

  case res of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 403 } })
      -> status status403 >> Scotty.json (object []) >> finish
    Left e -> do
      liftAndCatchIO $ putStr "Error: _SetPlaidAccount " >> print
        (user, e, accountInfo)
      status status500 >> finish
    Right _ -> return ()

  Scotty.json $ object []



