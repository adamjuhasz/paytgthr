{-|
Module      : App endpoints
Description : Endpoints for when the user has finished signing up
Maintainer  : adam@example.com
Stability   : experimental
-}
module LandingPage.Flows.Application
  ( appFlow
  ) where

import qualified Data.Vault.Lazy               as V
import           LandingPage.Blaze.Common       ( FlowStory(..) )
import           LandingPage.Handlers.Application.Cards
                                                ( activateCard
                                                , createNewCard
                                                , getCardInfo
                                                , getUserCards
                                                , lockCard
                                                , unlockCard
                                                )
import           LandingPage.Handlers.Application.Consent
                                                ( reConsentHandler )
import           LandingPage.Handlers.Application.CurrentLevel
                                                ( getCurrentLevel )
import           LandingPage.Handlers.Application.CurrentState
                                                ( getCurrentState )
import           LandingPage.Handlers.Application.FS.Plaid.ChooseAccount
                                                ( chooseAccountWithPlaid )
import           LandingPage.Handlers.Application.FS.Plaid.GetLinkToken
                                                ( getLinkToken )
import           LandingPage.Handlers.Application.GroupAccept
                                                ( acceptGroup )
import           LandingPage.Handlers.Application.GroupClose
                                                ( closeGroup )
import           LandingPage.Handlers.Application.GroupCreate
                                                ( createGroup )
import           LandingPage.Handlers.Application.GroupGet
                                                ( getSpecificGroup )
import           LandingPage.Handlers.Application.GroupList
                                                ( getGroupList )
import           LandingPage.Handlers.Application.GroupSplit
                                                ( getCategorySplits
                                                , setCategorySplits
                                                , setSplit
                                                )
import           LandingPage.Handlers.Application.Invite
                                                ( acceptInvite
                                                , getInviteCode
                                                )
import           LandingPage.Handlers.Application.Ledger
                                                ( getLedger )
import           LandingPage.Handlers.Application.Payments
                                                ( getRecentPayments )
import           LandingPage.Handlers.Application.Pin
                                                ( pinChangeHandler )
import           LandingPage.Handlers.Application.Referral
                                                ( getMyRefereesProgress
                                                , getMyReferralProgress
                                                , getMyReferralcode
                                                , useAReferralCode
                                                )
import           LandingPage.Handlers.Application.RequestStatement
                                                ( requestStatement )
import           LandingPage.Handlers.Application.Rewards
                                                ( activateReward
                                                , getActiveRewards
                                                , getOurRewards
                                                , getRewardEntries
                                                , transferRewardBalance
                                                )
import           LandingPage.Handlers.Application.Tokens
                                                ( createToken
                                                , verifyToken
                                                )
import           LandingPage.Handlers.Login     ( loginHandler )
import           LandingPage.Handlers.Login.ForgotPassword
                                                ( resetHandler )
import           LandingPage.Handlers.Login.Logout
                                                ( logoutHandler )
import           LandingPage.Handlers.Signup.Bank.LinkType.Renderer
                                                ( linkChooserRenderer )
import           LandingPage.Handlers.Statements.RenderStatements
                                                ( get12MonthStatement
                                                , get24MonthStatement
                                                , getAnyUserStatement
                                                )
import           LandingPage.Handlers.Transactions.API
                                                ( transactionAPIResponse )
import           LandingPage.Types              ( ClusterEnvironment
                                                , EncryptedPin
                                                , SessionData(..)
                                                )
import           LandingPage.Utils              ( AcceptType(..)
                                                , getAcceptType
                                                )
import           Servant.Client                 ( ClientEnv )
import           Shared.Amqp                    ( AMQPPublisher )
import           Shared.Vault                   ( PlainText )
import qualified Web.ClientSession             as WCS
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , ScottyM
                                                , get
                                                , next
                                                , post
                                                , redirect
                                                )

type PINEncrypter = PlainText -> IO EncryptedPin

appFlow
  :: ActionM AMQPPublisher
  -> V.Key SessionData
  -> WCS.Key
  -> PINEncrypter
  -> ClusterEnvironment
  -> ClientEnv
  -> ClientEnv
  -> ClientEnv
  -> ScottyM ()
appFlow publisherFactory sKey wcsKey pinEncrypter env accountsEnv privacyEnv paymentEnv
  = do
    get "/app/signup" $ redirect "/getapp"

    post "/app/login" $ loginHandler env accountsEnv wcsKey sKey
    -- post "/app/signup" $ signupHandler env wcsKey sKey accountsEnv
    post "/app/password/reset" $ resetHandler accountsEnv

    post "/app/logout" $ logoutHandler env wcsKey sKey

    get "/app/user/state" $ getCurrentState sKey Nothing accountsEnv
    get "/app/user/level" $ getCurrentLevel sKey paymentEnv accountsEnv
    get "/app/user/groups" $ getGroupList sKey accountsEnv
    post "/app/group/create" $ createGroup sKey accountsEnv
    get "/app/group/:id" $ getSpecificGroup sKey accountsEnv
    post "/app/group/:id/close" $ closeGroup sKey accountsEnv
    post "/app/group/:id/accept" $ acceptGroup sKey accountsEnv
    post "/app/group/:id/change/split" $ setSplit sKey accountsEnv
    get "/app/group/:id/set/split/categories"
      $ getCategorySplits sKey accountsEnv
    post "/app/group/:id/set/split/categories"
      $ setCategorySplits sKey accountsEnv

    get "/app/change/fs/ach/plaid/linktoken" $ getLinkToken sKey paymentEnv
    post "/app/change/fs/ach/plaid/chooseaccount"
      $ chooseAccountWithPlaid sKey paymentEnv

    get "/statements/months/12" $ publisherFactory >>= get12MonthStatement sKey
    get "/statements/months/24" $ publisherFactory >>= get24MonthStatement sKey
    get "/statements/user/:id/months/:length"
      $   publisherFactory
      >>= getAnyUserStatement

    post "/app/user/requestStatement"
      $   publisherFactory
      >>= requestStatement sKey

    get "/app/transactions" $ do
      accepts <- getAcceptType
      case accepts of
        ApplicationJSON -> transactionAPIResponse sKey accountsEnv paymentEnv
        TextHTML        -> next

    get "/app/payments/recent" $ getRecentPayments sKey accountsEnv paymentEnv
    get "/app/ledger/recent" $ getLedger sKey accountsEnv paymentEnv

    post "/app/user/legalconsent" $ reConsentHandler sKey accountsEnv

    get "/app/cards" $ getUserCards sKey accountsEnv
    get "/app/card/:cardid" $ getCardInfo sKey accountsEnv privacyEnv
    post "/app/card/:cardid/lock" $ lockCard sKey accountsEnv
    post "/app/card/:cardid/unlock" $ unlockCard sKey accountsEnv
    post "/app/card/:cardid/activate" $ activateCard sKey accountsEnv
    post "/app/card/:cardid/set/pin"
      $ pinChangeHandler pinEncrypter sKey accountsEnv
    post "/app/card/new/:type" $ createNewCard sKey accountsEnv

    get "/app/change/bank"
      $   publisherFactory
      >>= linkChooserRenderer sKey AppFlow

    post "/app/token/:medium/create/" $ createToken accountsEnv
    post "/app/token/:medium/verify/" $ verifyToken sKey accountsEnv

    get "/app/invite/code" $ getInviteCode sKey accountsEnv
    post "/app/invite/accept" $ acceptInvite sKey accountsEnv

    get "/app/rewards/all" $ getActiveRewards accountsEnv
    get "/app/rewards/ours" $ getOurRewards sKey accountsEnv
    post "/app/rewards/activate" $ activateReward sKey accountsEnv
    get "/app/rewards/entries" $ getRewardEntries sKey paymentEnv
    post "/app/rewards/payout/trasfer" $ transferRewardBalance sKey paymentEnv

    get "/app/referral/code" $ getMyReferralcode sKey accountsEnv
    get "/app/referral/progress" $ getMyReferralProgress sKey accountsEnv
    get "/app/referral/referee" $ getMyRefereesProgress sKey accountsEnv
    post "/app/referral/link" $ useAReferralCode sKey accountsEnv
