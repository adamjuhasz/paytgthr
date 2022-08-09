{-|

Allow user to select account link type
 -}

module LandingPage.Handlers.Signup.Bank.LinkType.Renderer where

import           Data.Functor
import qualified Data.Vault.Lazy               as V
import           LandingPage.Blaze.Common
import           LandingPage.Handlers.Router
import           LandingPage.Types
import           LandingPage.Utils
import           Network.Wai
import           Shared.Amqp
-- import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
-- import           Text.Blaze.Html5.Attributes   as HA
import           Text.Blaze.Html.Renderer.Text
import           Web.Scotty                    as Scotty

linkChooserRenderer
  :: V.Key SessionData -> FlowStory -> AMQPPublisher -> ActionM ()
linkChooserRenderer sKey currentFlow pub = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  (_, userModel, _) <- liftAndCatchIO $ routerBase pub user

  let
    plaidLink = case currentFlow of
      SignupFlow -> "/app/signup/plaid"
      AppFlow    -> "/app/change/plaid"
      NoSidebar  -> "/app/signup/plaid"
    manualLink = case currentFlow of
      SignupFlow -> "/app/signup/manuallink/entry"
      AppFlow    -> "/app/signup/manuallink/entry"
      NoSidebar  -> "/app/signup/plaid"
    pageCenterContent = do
      H.p "How would you like to link your bank account?"
      optionBox
        GetAction
        plaidLink
        False
        "Direct Link"
        [ CenterRow
          "Use Plaid to directly link your checking or savings account"
        , CenterRow
          "The spending limit of your Tgthr card will be the balance of your linked account"
        , CenterRow "(Not all banks supported)"
        ]
      optionBox
        GetAction
        manualLink
        False
        "Manual Link"
        [ CenterRow
          "You'll enter your banking details and we'll verify them with a small despoit"
        , CenterRow
          "As you make more purchases we'll raise the spending limit of your Tgthr card"
        , CenterRow "(Takes 2 - 3 days)"
        ]

  let rendered = renderHtml $ pageWithOptions currentFlow
                                              userModel
                                              EnterBanking
                                              "Link bank"
                                              pageCenterContent
  Scotty.html rendered
