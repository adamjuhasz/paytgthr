module LandingPage.Handlers.Signup.Disclosure where

import           Data.Aeson                     ( KeyValue(..)
                                                , Value
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( AcceptType
                                                  ( ApplicationJSON
                                                  , TextHTML
                                                  )
                                                , createTrace
                                                , expectSession
                                                , getAcceptType
                                                , requireUser
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(_UserAcceptDisclosure)
                                                , asClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , next
                                                , request
                                                )

disclosureRenderer :: [(Text, Value)] -> ActionM ()
disclosureRenderer inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object inParams
    TextHTML        -> Scotty.next

disclosureHandler :: V.Key SessionData -> ClientEnv -> ActionM ()
disclosureHandler sKey env = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser

  trace <- createTrace
  let fn = _UserAcceptDisclosure asClientM trace user

  res <- liftAndCatchIO $ runClientM fn env
  case res of
    Left e -> error $ "Error: disclosureHandler _UserAcceptDisclosure " <> show
      (user, e, trace)
    Right _ -> return ()

  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object ["success" .= True]
    TextHTML        -> Scotty.next
