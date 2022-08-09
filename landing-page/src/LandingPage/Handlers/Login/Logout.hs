module LandingPage.Handlers.Login.Logout where

import           Data.Functor                   ( (<&>) )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( ClusterEnvironment
                                                , SessionData(userID)
                                                )
import           LandingPage.Utils              ( expectSession
                                                , genTokenHeader
                                                )
import           Network.Wai                    ( Request(vault) )
import           Prelude                 hiding ( print
                                                , putStr
                                                , putStrLn
                                                ) -- Safety in not loggin credentials accidently
import qualified Web.ClientSession             as WCS
import           Web.Scotty                     ( ActionM
                                                , redirect
                                                , request
                                                )

logoutHandler
  :: ClusterEnvironment -> WCS.Key -> V.Key SessionData -> ActionM ()
logoutHandler env wcsKey sKey = do
  session <- request <&> vault <&> V.lookup sKey <&> expectSession

  genTokenHeader env wcsKey (session { userID = Nothing }) >> redirect "/"
