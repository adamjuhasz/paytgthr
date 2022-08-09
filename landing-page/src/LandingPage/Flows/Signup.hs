module LandingPage.Flows.Signup
  ( signUpFlow
  ) where

import           Data.Aeson                     ( Value )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Blaze.Common       ( FlowStory(..) )
import           LandingPage.Handlers.Router    ( routerBase )
import           LandingPage.Handlers.Signup.Activate
                                                ( cardActiveRenderer
                                                , handleCardActivate
                                                , renderCardActivate
                                                )
import           LandingPage.Handlers.Signup.Bank.LinkType.Renderer
                                                ( linkChooserRenderer )
import           LandingPage.Handlers.Signup.Consent
                                                ( consentDenyRenderer
                                                , consentRenderer
                                                , consetHandler
                                                )
import           LandingPage.Handlers.Signup.DateOfBirthUpdater
                                                ( dobUpdater )
import           LandingPage.Handlers.Signup.Disclosure
                                                ( disclosureHandler
                                                , disclosureRenderer
                                                )
import           LandingPage.Handlers.Signup.EmailUpdater
                                                ( emailUpdater )
import           LandingPage.Handlers.Signup.EmailVerifier
                                                ( emailVerifier )
import           LandingPage.Handlers.Signup.GeneralUpdates
                                                ( addressRenderer
                                                , nameRenderer
                                                , piiRenderer
                                                , userUpdateHandler
                                                )
import           LandingPage.Handlers.Signup.ManualLink.Handler
                                                ( handleEntry
                                                , handleVerification
                                                )
import           LandingPage.Handlers.Signup.PhoneUpdater
                                                ( phoneUpdater )
import           LandingPage.Handlers.Signup.PhoneVerifier
                                                ( phoneVerifier )
import           LandingPage.Handlers.Signup.SSNUpdater
                                                ( ssnUpdater )
import           LandingPage.Handlers.Signup.Waiting
                                                ( renderWaiting )
import           LandingPage.Templating.TemplateParams
                                                ( templateParamsForUserModel )
import           LandingPage.Types              ( EncryptedPin
                                                , EncryptedSSN
                                                , SessionData
                                                )
import           LandingPage.Utils              ( expectSession
                                                , requireUserNotClosed
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv )
import           Shared.Amqp                    ( AMQPPublisher )
import           Shared.Vault                   ( PlainText )
import           Web.Scotty                     ( ActionM
                                                , ScottyM
                                                , get
                                                , liftAndCatchIO
                                                , post
                                                , request
                                                )

type Renderer = [(Text, Value)] -> ActionM ()
type SSNEncrypter = PlainText -> IO EncryptedSSN
type PINEncrypter = PlainText -> IO EncryptedPin

signUpFlow
  :: ActionM AMQPPublisher
  -> V.Key SessionData
  -> SSNEncrypter
  -> PINEncrypter
  -> ClientEnv
  -> ClientEnv
  -> ScottyM ()
signUpFlow publisherFactory sKey ssnEncrypter pinEncrypter accountsEnv payAuthEnv
  = do
    let callRenderer' = callRenderer sKey

    post "/app/signup/email/verify" $ emailVerifier sKey accountsEnv
    post "/app/signup/email/change" $ emailUpdater sKey accountsEnv
    post "/app/signup/phone" $ phoneUpdater sKey accountsEnv
    post "/app/signup/phone/verify" $ phoneVerifier sKey accountsEnv
    post "/app/signup/phone/change" $ phoneUpdater sKey accountsEnv
    post "/app/signup/dob" $ dobUpdater sKey accountsEnv
    post "/app/signup/ssn" $ ssnUpdater ssnEncrypter sKey accountsEnv

    get "/app/signup/name" $ publisherFactory >>= callRenderer' nameRenderer
    post "/app/signup/name"
      $ userUpdateHandler ssnEncrypter sKey nameRenderer accountsEnv

    get "/app/signup/disclosures"
      $   publisherFactory
      >>= callRenderer' disclosureRenderer
    post "/app/signup/disclosures" $ disclosureHandler sKey accountsEnv
    get "/app/signup/pii" $ publisherFactory >>= callRenderer' piiRenderer
    post "/app/signup/pii"
      $ userUpdateHandler ssnEncrypter sKey piiRenderer accountsEnv

    get "/app/signup/address"
      $   publisherFactory
      >>= callRenderer' addressRenderer
    post "/app/signup/address"
      $ userUpdateHandler ssnEncrypter sKey addressRenderer accountsEnv

    -- Choose link type
    get "/app/signup/link"
      $   publisherFactory
      >>= linkChooserRenderer sKey SignupFlow

    -- Manual Link
    post "/app/signup/manuallink/entry"
      $ handleEntry sKey accountsEnv payAuthEnv
    post "/app/signup/manuallink/verify"
      $ handleVerification sKey accountsEnv payAuthEnv

    -- Conseent
    get "/app/signup/legalconsent"
      $   publisherFactory
      >>= callRenderer' consentRenderer
    get "/app/signup/legalconsent/deny"
      $   publisherFactory
      >>= callRenderer' consentDenyRenderer
    post "/app/signup/legalconsent" $ consetHandler sKey accountsEnv

    -- KYC Section
    get "/app/signup/kyc" $ publisherFactory >>= renderWaiting sKey

    -- Activate Card Section
    get "/app/signup/pin"
      $   publisherFactory
      >>= callRenderer' renderCardActivate
    post "/app/signup/pin"
      $   publisherFactory
      >>= handleCardActivate pinEncrypter sKey

    get "/app/signup/cardactive"
      $   publisherFactory
      >>= callRenderer' cardActiveRenderer

callRenderer :: V.Key SessionData -> Renderer -> AMQPPublisher -> ActionM ()
callRenderer sKey renderer pub = do
  user <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosed pub
  (_, thisUser, _) <- liftAndCatchIO $ routerBase pub user
  renderer (templateParamsForUserModel thisUser)
