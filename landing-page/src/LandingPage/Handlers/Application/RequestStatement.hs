module LandingPage.Handlers.Application.RequestStatement where

import           Data.Aeson                     ( object )
import           Data.Functor                   ( (<&>) )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( expectSession
                                                , requireUser
                                                )
import           Network.Wai                    ( Request(vault) )
import           Shared.Amqp                    ( AMQPPublisher
                                                , CommandMessage(PayCmd)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PaymentCmd(RequestStatement) )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                )


requestStatement :: V.Key SessionData -> AMQPPublisher -> ActionM ()
requestStatement sKey pub = do
  uid <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser

  _   <- liftAndCatchIO $ pub $ PayCmd $ RequestStatement uid Nothing Nothing

  Scotty.json $ object []
