{-|

Blaze templates
 -}
{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE RecordWildCards, QuasiQuotes #-}

module LandingPage.Blaze.Common where

import           Control.Monad
import           Data.Coerce
import           Data.Text                      ( Text )
import           Data.Maybe
import           Data.UUID                      ( toText )
import           LandingPage.Types
import           Shared.Models.User
import           Text.Blaze.Html5               ( Html
                                                , Attribute
                                                , AttributeValue
                                                , (!)
                                                , customAttribute
                                                , preEscapedToHtml
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as HA
import qualified Text.QuasiText                as QT
import           Text.RawString.QQ

data FlowStory
  = SignupFlow
  | AppFlow
  | NoSidebar
  deriving (Eq, Show)

segmentBlock :: Html
segmentBlock = preEscapedToHtml fn
 where
  fn :: Text = [r|
  function runSegment(writeKey) {
    var analytics = window.analytics = window.analytics || []; if (!analytics.initialize) if (analytics.invoked) window.console && console.error && console.error("Segment snippet included twice."); else {
      analytics.invoked = !0; analytics.methods = ["trackSubmit", "trackClick", "trackLink", "trackForm", "pageview", "identify", "reset", "group", "track", "ready", "alias", "debug", "page", "once", "off", "on"]; analytics.factory = function (t) { return function () { var e = Array.prototype.slice.call(arguments); e.unshift(t); analytics.push(e); return analytics } }; for (var t = 0; t < analytics.methods.length; t++) { var e = analytics.methods[t]; analytics[e] = analytics.factory(e) } analytics.load = function (t, e) { var n = document.createElement("script"); n.type = "text/javascript"; n.async = !0; n.src = "https://cdn.segment.com/analytics.js/v1/" + t + "/analytics.min.js"; var a = document.getElementsByTagName("script")[0]; a.parentNode.insertBefore(n, a); analytics._loadOptions = e }; analytics.SNIPPET_VERSION = "4.1.0";
      analytics.load(writeKey);
      analytics.page();
    }
  };

  switch (location.hostname) {
    case "paytgthr.com":
      runSegment("Buy7YnkSuL5ic62Mj3D9kPHJ4xbbUWgO");
      break;
    case "paytgthr.dev":
      runSegment("GJur9X8qHgxlJe1evjwMlSBpf5Js5BvZ");
      break;
    case "localhost":
      runSegment("Wd2uWVV0VKchcUmiLqnxYuOohGDY8c8U");
      break;
    default:
      break;
  }
|]

signUpHelpers :: Html
signUpHelpers = preEscapedToHtml jsCode
 where
  -- Javascript Start
  jsCode :: Text = [r|
  function genFormatter(firstIndex, secondIndex) {
    return function (e) {
      var cursorStart = e.target.selectionStart,
          cursorEnd = e.target.selectionEnd,
          origVal = e.target.value;

      formatInput(firstIndex, secondIndex, e.target, e.inputType);
      
      if (cursorStart != origVal.length) {
        e.target.setSelectionRange(cursorStart, cursorEnd);
      }
    }
  }

  function formatInput(firstIndex, secondIndex, target, inputType) {
    var origVal = target.value;
    var val = origVal.replace(/\D/g, '');
    var first = val.slice(0, firstIndex);
    var second = val.slice(firstIndex, secondIndex);
    var third = val.slice(secondIndex);

    var newVal = first;
    if (first.length == firstIndex && !(inputType == "deleteContentBackward" && second.length == 0)) {
      newVal = newVal.concat("-");
    }
    newVal = newVal.concat(second);
    if (second.length == (secondIndex - firstIndex) && !(inputType == "deleteContentBackward" && third.length == 0)) {
      newVal = newVal.concat("-");
    }
    newVal = newVal.concat(third);

    target.value = newVal;
  }

  var elements = document.querySelectorAll('form');
  elements.forEach(function(el) {
    el.addEventListener("submit", function(_evt) {
      el.disabled = true; // html5 Only
      //backup: disable all buttons too
      var buttons = el.querySelectorAll('*[type="submit"]');
      buttons.forEach(function(aButton) {
        aButton.disabled = true;
        spinners = aButton.querySelectorAll('.spin-dual-ring');
        spinners.forEach(function(spinner) {
          spinner.style.display = "initial";
        });
      });
    });
  });
  
  /**
  * If browser back button was used, flush cache
  * This ensures that user will always see an accurate, up-to-date view based on their state 
  **/
  (function () {
    window.onpageshow = function(event) {
      if (event.persisted) {
        window.location.reload();
      }
    };
  })();
|]
-- Javascript END

analyticsCode :: UserModel -> Html
analyticsCode UserModel {..} = preEscapedToHtml jsCode
 where
  jsString mt = case mt of
    Nothing -> "undefined"
    Just t  -> "'" <> t <> "'"
  userid :: Text    = toText . fromUserID $ usrUserID
  firstName :: Text = jsString usrFirstName
  lastName :: Text  = jsString usrLastName
  email :: Text     = "'" <> coerce usrEmail <> "'"
  -- Javascript Start
  jsCode = [QT.embed|
  try {
    analytics.ready(function() {
      if (analytics.user().id() == null) {
        analytics.alias('$userid', function() {
          setTimeout(function() {
            analytics.identify('$userid',{
              firstName: $firstName,
              lastName: $lastName,
              email: $email
            });
          }, 1000);
        });
        console.log("aliasing user");
      } else {
        analytics.identify('$userid',{
          firstName: $firstName,
          lastName: $lastName,
          email: $email
        });
      }
    });
  } catch (e) {
    console.error(e);
  }
|]
-- Javascript End

trackForm :: UserModel -> Html
trackForm UserModel {..} = preEscapedToHtml jsCode
 where
  jsString mt = case mt of
    Nothing -> "undefined"
    Just t  -> "'" <> t <> "'"
  userid :: Text    = toText . fromUserID $ usrUserID
  firstName :: Text = jsString usrFirstName
  lastName :: Text  = jsString usrLastName
  email :: Text     = "'" <> coerce usrEmail <> "'"
  -- Javascript Start
  jsCode = [QT.embed|
  try {
    analytics.ready(function() {
      if (analytics.user().id() == null) {
        analytics.alias('$userid', function() {
          setTimeout(function() {
            analytics.identify('$userid',{
              firstName: $firstName,
              lastName: $lastName,
              email: $email
            });
          }, 1000);
        });
        console.log("aliasing user");
      } else {
        analytics.identify('$userid',{
          firstName: $firstName,
          lastName: $lastName,
          email: $email
        });
      }
    });
  } catch (e) {
    console.error(e);
  }
|]
-- Javascript End

inputmode :: AttributeValue -> Attribute
inputmode = customAttribute "inputmode"

sharedHead :: Html
sharedHead = do
  H.script ! type_ "text/javascript" $ segmentBlock
  H.meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
  H.link ! rel "stylesheet" ! href "/css/normalize.css"
  H.link ! rel "stylesheet" ! href "/css/base.css"

linkHref :: Bool -> AttributeValue -> Attribute
linkHref condition hrefTo = if condition then href hrefTo else mempty

signupSidebar :: UserModel -> UserSignupStep -> Html
signupSidebar UserModel {..} currentStep =
  let
    nameiscurrent       = currentStep == EnterName
    nameIsDone          = isJust usrFirstName && isJust usrLastName
    -- partner
    partneriscurrent    = currentStep == EnterPartner
    partnrIsDone        = currentStep > EnterPartner
    -- disclosure
    disclosureiscurrent = currentStep == AcceptDisclosures
    disclosureIsDone    = isJust usrDislcosureOk
    -- pii
    piiiscurrent        = currentStep == EnterPII
    piiIsDone           = isJust usrPhone && isJust usrDOB && isJust usrSSN
    -- address
    addressiscurrent    = currentStep == EnterAddress
    addressIsDone       = isJust usrAddressStreet && isJust usrAddressZip
    -- bank 
    bankiscurrent =
      (currentStep == EnterBanking)
        || (currentStep == PlaidLink)
        || (currentStep == ChooseChecking)
        || (currentStep == ManualBankEntry)
        || (currentStep == VerifyBankWaiting)
        || (currentStep == VerifyManualBank)
    bankIsDone       = isJust usrBankName
    -- legal consent
    consentiscurrent = currentStep == AcceptPolicies
    consentIsDone    = isJust usrConstentOk
    -- ratio
    ratioiscurrent   = currentStep == SetRatio
    ratioisprevious  = currentStep > SetRatio
    -- kyc
    kyciscurrent     = currentStep >= WaitOnKYC
    kycIsDone        = isJust usrAptoKYCStatus
  in
    do
      H.div ! class_ "side-menu" $ do
        H.div ! class_ "signupSidebar-header" $ do
          H.a ! href "/" $ do
            H.img ! src "/images/paytgthr.svg" ! alt "pay tgthr logo"
        H.div ! class_ "side-list" $ do
          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if nameIsDone then " isprevious " else "")
                <> (if nameiscurrent then " iscurrent " else "")
                )
            ! linkHref (not nameiscurrent) "/app/signup/name"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Your name"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if partnrIsDone then " isprevious " else "")
                <> (if partneriscurrent then " iscurrent " else "")
                )

            ! linkHref (not partneriscurrent) "/app/signup/partner"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Partner's name & email"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if ratioisprevious then " isprevious " else "")
                <> (if ratioiscurrent then " iscurrent " else "")
                )
            ! linkHref (not ratioiscurrent) "/app/signup/ratio"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Choose purchase split"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if disclosureIsDone then " isprevious " else "")
                <> (if disclosureiscurrent then " iscurrent " else "")
                )
            ! linkHref (not disclosureiscurrent) "/app/signup/disclosures"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Disclosures"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if consentIsDone then " isprevious " else "")
                <> (if consentiscurrent then " iscurrent " else "")
                )
            ! linkHref (not consentiscurrent) "/app/signup/legalconsent"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Legal consent"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if piiIsDone then " isprevious " else "")
                <> (if piiiscurrent then " iscurrent " else "")
                )
            ! linkHref (not piiiscurrent) "/app/signup/pii"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Personal information"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if addressIsDone then " isprevious " else "")
                <> (if addressiscurrent then " iscurrent " else "")
                )
            ! linkHref (not addressiscurrent) "/app/signup/address"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Residential address"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if bankIsDone then " isprevious " else "")
                <> (if bankiscurrent then " iscurrent " else "")
                )
            ! linkHref (not bankiscurrent) "/app/signup/plaid"
            $ do
                H.div ! class_ "current-marker" $ mempty
                H.div ! class_ "complete-marker" $ "✔️"
                H.div ! class_ "side-list-entry-text" $ "Link bank account"

          H.a
            ! class_
                (  "side-list-entry no-underline"
                <> (if kycIsDone then " isprevious " else "")
                <> (if kyciscurrent then " iscurrent " else "")
                )
            $ do
                H.div ! class_ "final-marker" $ "👍"
                H.div ! class_ "side-list-entry-text" $ "Complete"

        H.div ! class_ "side-footer" $ do
          H.a ! href "/app/logout" $ "Logout"

appSidebar :: UserModel -> UserSignupStep -> Html
appSidebar _ _ = do
  H.div ! class_ "side-menu" $ do
    H.div ! class_ "signupSidebar-header" $ do
      H.a ! href "/" $ do
        H.img ! src "/images/paytgthr.svg" ! alt "pay tgthr logo"

    H.div ! class_ "side-list" $ do

      H.div ! class_ "side-footer" $ do
        H.a ! href "/app/logout" $ "Logout"

header :: UserSignupStep -> Html -> Html
header currentStep pageTitle =
  let backHref = backLinkForStep currentStep
  in  do
        H.div ! class_ "content-header" $ do
          H.a ! class_ "back-button" ! href backHref $ do
            H.img ! class_ "back-chevron" ! src "/images/arrow-back.svg"
            H.div ! class_ "back-text font-small" $ "Back"
          H.span ! class_ "page-title font-medium" $ toHtml pageTitle
          H.div ! class_ "back-button invisible" $ do
            H.img ! class_ "back-chevron" ! src "/images/arrow-back.svg"

backgroundColorForStep :: UserSignupStep -> (AttributeValue, AttributeValue)
backgroundColorForStep currentStep =
  let purple = ("#a069ff", "purple")
      green  = ("#65dba2", "green")
  in  case currentStep of
        ManualBankEntry   -> purple
        VerifyBankWaiting -> green
        VerifyManualBank  -> purple
        _                 -> purple

backLinkForStep :: UserSignupStep -> AttributeValue
backLinkForStep currentStep = toValue $ previousPageForStep currentStep

primaryButton :: Html -> Html
primaryButton buttonText = do
  H.button ! type_ "submit" ! class_ "primary" $ do
    H.div ! class_ "spin-dual-ring" $ mempty
    H.span buttonText

pageWithOptions
  :: FlowStory -> UserModel -> UserSignupStep -> Html -> Html -> Html
pageWithOptions flow userModel currentStep pageTitle pageContent =
  let (bgColor, bgClass) = backgroundColorForStep currentStep
      sidebar            = case flow of
        SignupFlow -> signupSidebar userModel currentStep
        AppFlow    -> appSidebar userModel currentStep
        NoSidebar  -> mempty
  in  do
        H.docType
        H.html ! lang "en" $ do
          --head
          H.head $ do
            H.title pageTitle
            H.meta ! name "theme-color" ! content bgColor
            sharedHead
          -- body
          H.body ! class_ bgClass $ do
            H.div ! class_ ("app-frame framer background " <> bgClass) $ do
              sidebar
              H.div ! class_ "main-content column-stack" $ do
                case flow of
                  NoSidebar -> mempty
                  _         -> header currentStep pageTitle
                H.div ! class_ "content-frame" $ do
                  H.div ! class_ "select-parent" $ do
                    pageContent
                H.div ! class_ "empty" $ mempty
            -- JS scripts (footer)
            H.script signUpHelpers
            H.script $ analyticsCode userModel

data FormMethod = GetAction | PostAction deriving (Eq, Show)
data OptionRow
  = LeftRightRow
    { rowName :: Html
    , rowContent :: Html
    }
  | CenterRow
    { rowDetails :: Html
    }

optionBox :: FormMethod -> AttributeValue -> Bool -> Html -> [OptionRow] -> Html
optionBox methodAction actionLoc isDisabled headerText infoRows =
  let formMethod = case methodAction of
        GetAction  -> method "get"
        PostAction -> method "post"
      boxClasses =
          "select-box checkingbox" <> if isDisabled then "formdisabled" else ""
      rowGen CenterRow {..} = do
        H.div ! class_ "account-info-row" $ rowDetails
      rowGen LeftRightRow {..} = do
        H.div ! class_ "account-info-row" $ do
          H.div ! class_ "info-explain" $ rowName
          H.div rowContent
  in  do
        H.form ! action actionLoc ! formMethod ! class_ boxClasses $ do
          H.div ! class_ "select-text" $ headerText
          mapM_ rowGen infoRows
          unless isDisabled (primaryButton "Select")

data ButtonLink
  = PrimaryLink
    { linkText :: Html
    , linkTo :: AttributeValue
    }
  | JavascriptButton
    { linkText :: Html
    , linkTo :: AttributeValue
    }

data PageDescription
  = JustTextPage
    { flow :: FlowStory
    , bottomButton :: ButtonLink
    , pageTitle :: Html
    , pageContent :: Html
    , headerHtml :: Html
    , footerHtml :: Html
    }
  | PageWithForm
    { flow :: FlowStory
    , pageTitle :: Html
    , pageContent :: Html
    , headerHtml :: Html
    , footerHtml :: Html
    , formAction :: AttributeValue
    , trackEvent :: Text
    }

commmonPage :: UserModel -> UserSignupStep -> PageDescription -> Html
commmonPage userModel currentStep JustTextPage {..} =
  let (bgColor, bgClass) = backgroundColorForStep currentStep
      sidebar            = case flow of
        SignupFlow -> signupSidebar userModel currentStep
        AppFlow    -> appSidebar userModel currentStep
        NoSidebar  -> mempty
      theButton = case bottomButton of
        PrimaryLink {..} -> do
          H.a ! class_ "no-underline" ! href linkTo ! HA.id "main-button" $ do
            H.div ! class_ "primary button" $ linkText
        JavascriptButton {..} -> do
          H.a
            ! class_ "no-underline"
            ! onclick linkTo
            ! HA.id "main-button"
            $ do
                H.div ! class_ "primary button" $ linkText
  in  do
        H.docType
        H.html ! lang "en" $ do
          --head
          H.head $ do
            H.title pageTitle
            H.meta ! name "theme-color" ! content bgColor
            sharedHead
            headerHtml
          -- body
          H.body ! class_ bgClass $ do
            H.div ! class_ ("app-frame framer background " <> bgClass) $ do
              sidebar
              H.div ! class_ "main-content column-stack" $ do
                case flow of
                  NoSidebar -> mempty
                  _         -> header currentStep pageTitle
                H.div ! class_ "content-frame center-flex-column" $ do
                  H.div ! class_ "center-flex-column" $ do
                    pageContent
                H.div ! class_ "button-frame" $ do
                  theButton
            -- JS scripts (footer)
            footerHtml

            H.script signUpHelpers
            H.script $ analyticsCode userModel

commmonPage userModel currentStep PageWithForm {..} =
  let
    (bgColor, bgClass) = backgroundColorForStep currentStep
    sidebar            = case flow of
      SignupFlow -> signupSidebar userModel currentStep
      AppFlow    -> appSidebar userModel currentStep
      NoSidebar  -> mempty
  in
    do
      H.docType
      H.html ! lang "en" $ do
        --head
        H.head $ do
          H.title pageTitle
          H.meta ! name "theme-color" ! content bgColor
          sharedHead
          headerHtml
        -- body
        H.body ! class_ bgClass $ do
          H.div ! class_ ("app-frame framer background " <> bgClass) $ do
            sidebar
            H.form
              ! action formAction
              ! method "post"
              ! class_ "main-content column-stack"
              ! HA.id "main-form"
              $ do
                  case flow of
                    NoSidebar -> mempty
                    _         -> header currentStep pageTitle
                  H.div
                    ! class_ "content-frame fluid-form center-flex-column"
                    $ do
                        pageContent
                  H.div ! class_ "button-frame" $ do
                    primaryButton "Continue"
          -- JS scripts (footer)
          footerHtml
          H.script . preEscapedToHtml $ [QT.embed|
            try {
              var form = document.getElementById('main-form');
              analytics.trackForm(form, '$trackEvent', {});
            } catch(e) {
              console.error(e);
            }
          |]
          H.script signUpHelpers
          H.script $ analyticsCode userModel

errorInfoBox :: Html -> Html
errorInfoBox contents = do
  H.div ! class_ "info-box" $ do
    H.img ! class_ "info-icon" ! src "/images/error_info.svg"
    H.div ! class_ "info-content" $ contents
