module LandingPage.Flows.Tracking where

import           LandingPage.Handlers.Tracking.TrackConsole
                                                ( trackConsole )
import           LandingPage.Handlers.Tracking.TrackEvent
                                                ( trackEvent )
import           LandingPage.Handlers.Tracking.TrackWebEvent
                                                ( trackWebEvent )
import           LandingPage.Types              ( DBRunner )
import           Servant.Client                 ( ClientEnv )
import           Web.Scotty                     ( ScottyM
                                                , post
                                                )

trackingFlow
  :: DBRunner -> ClientEnv -> ScottyM ()
trackingFlow withDBPool  accountsEnv = do
  post "/record/console" trackConsole
  post "/record/event" $ trackEvent withDBPool accountsEnv
  post "/record/event/web" trackWebEvent
