{-# LANGUAGE RecordWildCards #-}

module LandingPage.Templating.TemplateParams where

import           Data.Aeson                     ( Value(..)
                                                , (.=)
                                                , object
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time
import           Shared.Models.User
import           LandingPage.Types

pageParams :: [(Text, Value)]
pageParams =
  [ ("transaction_href"   , String $ stepToPath Complete)
  , ("consent_href"       , String $ stepToPath AcceptPolicies)
  , ("consent_deny_href"  , "/app/signup/legalconsent/deny")
  , ("login_href"         , "/app/login")
  , ("signup_href"        , "/app/signup")
  , ("partner_manual_href", "/app/signup/partner/manual")
  , ("logout_href"        , "/app/logout")
  , ("settings_href"      , "/app/settings")
  , ("forgotpassword_href", "/app/password/forgot")
  , ("activate_href"      , String $ stepToPath Activate)
  , ("kyc_href"           , String $ stepToPath WaitOnKYC)
  , ("policies_href"      , String $ stepToPath ReviewLegal)
  , ("plaid_href"         , String $ stepToPath ChangeBank)
  , ("ratio_href"         , String $ stepToPath ChangeRatio)
  , ("pinchange_href"     , String $ stepToPath ChangePin)
  , ("disclosures_href"   , String $ stepToPath AcceptDisclosures)
  , ("partner_href"       , String $ stepToPath EnterPartner)
  , ("pii_href"           , String $ stepToPath EnterPII)
  , ("name_href"          , String $ stepToPath EnterName)
  , ("address_href"       , String $ stepToPath EnterAddress)
  , ("set_ratio_href"     , String $ stepToPath SetRatio)
  , ("signup_plaid_href"  , String $ stepToPath EnterBanking)
  ]

timeFormatter :: UTCTime -> Text
timeFormatter = T.pack . formatTime defaultTimeLocale "%m/%d/%0Y"

templateParamsForUserModel :: UserModel -> [(Text, Value)]
templateParamsForUserModel UserModel {..} =
  [ "user" .= object
    [ "userid" .= usrUserID
    , "firstName" .= usrFirstName
    , "lastName" .= usrLastName
    , "email" .= usrEmail
    ]
  , "fname-value" .= usrFirstName
  , "lname-value" .= usrLastName
  , "dob-value" .= (timeFormatter <$> usrDOB)
  , "phone-value" .= usrPhone
  , "street-value" .= usrAddressStreet
  , "street2-value" .= usrAddressStreet2
  , "city-value" .= usrAddressCity
  , "state-value" .= usrAddressState
  , "zip-value" .= usrAddressZip
  ]

templateParamsForPath :: UserSignupStep -> [(Text, Value)]
templateParamsForPath currentStep =
  pageParams
    <> ["back_href" .= previousPageForStep currentStep]
    <> [ ("isApp", "true") | currentStep > Complete ]

signupTemplateParams :: UserSignupStep -> UserSignupStep -> [(Text, Value)]
signupTemplateParams currentStep nextStep =
  templateParamsForPath currentStep
    <> ["next_href" .= stepToPath nextStep]
    -- name
    <> [ ("nameisnext", "true") | nextStep == EnterName ]
    <> [ ("nameiscurrent", "true") | currentStep == EnterName ]
    <> [ ("nameisprevious", "true") | nextStep > EnterName ]
    -- partner
    <> [ ("partnerisnext", "true") | nextStep == EnterPartner ]
    <> [ ("partneriscurrent", "true") | currentStep == EnterPartner ]
    <> [ ("partnerisprevious", "true") | nextStep > EnterPartner ]
    -- disclosure
    <> [ ("disclosureisnext", "true") | nextStep == AcceptDisclosures ]
    <> [ ("disclosureiscurrent", "true") | currentStep == AcceptDisclosures ]
    <> [ ("disclosureisprevious", "true") | nextStep > AcceptDisclosures ]
    -- pii
    <> [ ("piiisnext", "true") | nextStep == EnterPII ]
    <> [ ("piiiscurrent", "true") | currentStep == EnterPII ]
    <> [ ("piiisprevious", "true") | nextStep > EnterPII ]
    -- address
    <> [ ("addressisnext", "true") | nextStep == EnterAddress ]
    <> [ ("addressiscurrent", "true") | currentStep == EnterAddress ]
    <> [ ("addresseisprevious", "true") | nextStep > EnterAddress ]
    -- bank
    <> [ ("bankisnext", "true") | nextStep == EnterBanking ]
    <> [ ("bankiscurrent", "true")
       | (currentStep == EnterBanking)
         || (currentStep == PlaidLink)
         || (currentStep == ChooseChecking)
         || (currentStep == ManualBankEntry)
         || (currentStep == VerifyBankWaiting)
         || (currentStep == VerifyManualBank)
       ]
    <> [ ("bankisprevious", "true") | nextStep > ChooseChecking ]
    -- legal consent
    <> [ ("consentisnext", "true") | nextStep == AcceptPolicies ]
    <> [ ("consentiscurrent", "true") | currentStep == AcceptPolicies ]
    <> [ ("consentisprevious", "true") | nextStep > AcceptPolicies ]
    -- ratio
    <> [ ("ratioisnext", "true") | nextStep == SetRatio ]
    <> [ ("ratioiscurrent", "true") | currentStep == SetRatio ]
    <> [ ("ratioisprevious", "true") | nextStep > SetRatio ]
    -- kyc
    <> [ ("kyciscurrent", "true") | currentStep >= WaitOnKYC ]
    <> [ ("kycisprevious", "true") | nextStep > Complete ]
