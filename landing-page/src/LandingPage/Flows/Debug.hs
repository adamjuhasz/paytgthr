module LandingPage.Flows.Debug where

import qualified Data.Vault.Lazy               as V
import           LandingPage.Handlers.Debug     ( accountStatus
                                                , dumpSessionHandler
                                                , groupStatus
                                                )
import           LandingPage.Types              ( SessionData )
import           Shared.Amqp                    ( AMQPPublisher )
import           Web.Scotty                     ( ActionM
                                                , ScottyM
                                                , get
                                                )

debugFlow :: ActionM AMQPPublisher -> V.Key SessionData -> ScottyM ()
debugFlow publisher sKey = do
  get "/debug/showtoken" $ dumpSessionHandler sKey
  get "/debug/showstatus" $ publisher >>= accountStatus sKey
  get "/debug/showgroup" $ publisher >>= groupStatus sKey

