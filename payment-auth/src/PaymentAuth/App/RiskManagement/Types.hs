module PaymentAuth.App.RiskManagement.Types where

data MerchantRiskScore
  = None
  | WhitelistAllow
  | Low
  | Medium
  | High
  | VeryHigh
  deriving (Eq, Ord, Show)
