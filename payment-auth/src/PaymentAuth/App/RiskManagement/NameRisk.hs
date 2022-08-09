module PaymentAuth.App.RiskManagement.NameRisk where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           PaymentAuth.App.RiskManagement.Types
                                                ( MerchantRiskScore(..) )

isInfixOf :: Text -> Text -> Bool
isInfixOf a b = T.isInfixOf (T.toLower a) (T.toLower b)

nameRiskScore :: Text -> MerchantRiskScore
nameRiskScore merchantName = case (hasAllowedName, hasBlockedName) of
  (True , _    ) -> WhitelistAllow
  (False, True ) -> VeryHigh
  (False, False) -> None
 where
  hasBlockedName = or $ fmap (`isInfixOf` merchantName) blacklistedNames
  hasAllowedName = or $ fmap (`isInfixOf` merchantName) whitelistedNames
  blacklistedNames =
    [ "wish.com"            -- lots of frady users around this
    , "giftcard"            -- https://chewpaca.paytgthr.com/transaction/00000000-0000-0000-0000-000000000000
    , "cash app"            -- P2P
    , "venmo"               -- P2P
    , "xoom.com"            -- P2P
    , "moneygram"           -- P2P
    , "pay.fb.com"          -- P2P
    , "paypal *facebookpay" -- P2P
    , "facebookpay"         -- P2P
    , "jassby"              -- is a debit card (jassby.com)
    , "7-11wallet"          -- Is actually a ditigal wallet but has wrong MCC
    , "luckyland"           -- Online gambling
    , "funzpoints"          -- Online gambling
    , "chumba gold coins"   -- Online gambling
    , "quadpay"             -- A card card used to pay later
    , "zebit"               -- A card card used to pay later
    , "m1 finance"          -- A card card used to pay later
    , "zhang liang"         -- Online gambling
    , "zitobox"             -- Online gambling
    , "cleo ai"             -- Neobank
    ]
  whitelistedNames = ["affirm"]
