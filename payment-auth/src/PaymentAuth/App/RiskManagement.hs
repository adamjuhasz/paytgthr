{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.RiskManagement where

import           PaymentAuth.App.RiskManagement.MCCRisk
                                                ( mccRiskScore )
import           PaymentAuth.App.RiskManagement.NameRisk
                                                ( nameRiskScore )
import           PaymentAuth.App.RiskManagement.Types
                                                ( MerchantRiskScore(..) )
import           PaymentAuth.App.Split          ( calculateSplit )
import           Shared.Models.Currency         ( Currency(..)
                                                , getIsoCode
                                                , getMonetaryValue
                                                )
import           Shared.Models.RiskScore        ( RiskScore
                                                , UserLevel(..)
                                                , getUserLevel
                                                )
import           Shared.Models.Transaction      ( DeclineReason
                                                  ( ExceedMaxTrxAmount
                                                  , LowBalance
                                                  , P2PNotAllowed
                                                  , RiskyMerchant
                                                  )
                                                , MastercardMCC(MastercardMCC)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionState
                                                  ( TrxAuthorized
                                                  , TrxDeclined
                                                  )
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult
                                                  ( InsufficentFunds
                                                  , RiskTriggered
                                                  , Success
                                                  )
                                                , Balance
                                                , ExpectedShare
                                                , RiskRules(..)
                                                )

maxTransactionAmount :: Currency
maxTransactionAmount = Currency "USD" 1000

type ActualShare = Currency
type ReservedShare = Currency
type ShareTuple = (UserID, ActualShare, ReservedShare, Balance)

postBalance :: ShareTuple -> [ShareTuple] -> [ShareTuple]
postBalance t@(_, _, resShare, balance) accum =
  accum <> [ t | (balance - resShare) < Currency "USD" 0 ]

  -- inline brittany config for width
  -- brittany-next-binding --columns 90
approveTransaction
  :: RiskScore -> Transaction -> [(UserID, Currency)] -> (Transaction, AuthResult)
approveTransaction riskScore trx@Transaction {..} balances
  | null trxSplitAmounts
  = error "trxSplitAmounts is empty"
  | length zippedBalances /= length trxSplitAmounts
  = error "balances missing"
  | otherwise
  = case (isOverMaxTrxAmt, allHaveEnoughFunds, isRiskyMerchant, isTrustedUser) of
    (False, _    , _        , _    ) -> markTrxExceeded trx maxTransactionAmount
    (_    , False, _        , _    ) -> markInsufficent trx failEnoughFunds
    (_    , _    , Just risk, False) -> markRiskyMerchant trx risk
    (_    , _    , Just _   , True ) -> markApproved trx -- approve all risky merchants for trusted users
    (_    , _    , _        , _    ) -> markApproved trx
 where
  curryConv (a, b) = (a, Currency currISO b)
  currISO            = getIsoCode trxDisplayAmount
  currValue          = getMonetaryValue trxDisplayAmount
  fairShares         = curryConv <$> calculateSplit currValue trxSplitAmounts
  zippedBalances     = zipShareBalance balances fairShares
  failEnoughFunds    = foldr postBalance [] zippedBalances
  allHaveEnoughFunds = null failEnoughFunds
  isTrustedUser      = getUserLevel riskScore >= Level4
  isOverMaxTrxAmt    = trxDisplayAmount <= maxTransactionAmount
  isRiskyMerchant    = riskyMerchant trx

zipShareBalance
  :: [(UserID, Balance)] -> [(UserID, ExpectedShare)] -> [ShareTuple]
zipShareBalance balances finalShare = fmap zipUp balances
 where
  zipUp :: (UserID, Balance) -> ShareTuple
  zipUp b = (fst b, shareFind b, shareFind b, snd b)
  shareFind tup = errorNothing $ lookup (fst tup) finalShare
  errorNothing (Just x) = x
  errorNothing Nothing  = error "Nothing detected"

markTrxExceeded :: Transaction -> Currency -> (Transaction, AuthResult)
markTrxExceeded trx maxAmount =
  let trx' = trx { trxState = TrxDeclined (ExceedMaxTrxAmount maxAmount) }
  in  (trx', RiskTriggered trx' (ExceedMaxPerTrxAmount maxAmount))

markInsufficent :: Transaction -> [ShareTuple] -> (Transaction, AuthResult)
markInsufficent trx failed =
  let failedUsers  = (\(a, _, _, _) -> a) <$> failed
      failedShares = (\(u, s, _, b) -> (u, s, b)) <$> failed
      trx'         = trx { trxState = TrxDeclined (LowBalance failedUsers) }
  in  (trx', InsufficentFunds trx' failedShares)

markApproved :: Transaction -> (Transaction, AuthResult)
markApproved trx =
  let trx' = trx { trxState = TrxAuthorized } in (trx', Success trx')

markRiskyMerchant :: Transaction -> RiskRules -> (Transaction, AuthResult)
markRiskyMerchant trx risk =
  let reason = case risk of
        RiskyP2P                  -> P2PNotAllowed
        (ExceedMaxPerTrxAmount c) -> ExceedMaxTrxAmount c
        RiskyMerchantName         -> RiskyMerchant
        RiskyMerchantMCC          -> RiskyMerchant
        CombinedRiskRules         -> RiskyMerchant
        RiskyMerchantCountry      -> RiskyMerchant
      trx' = trx { trxState = TrxDeclined reason }
  in  (trx', RiskTriggered trx' risk)


data MerchantRisk
  = MerchantRisk RiskRules MerchantRiskScore
  | NoMerchantRisk
  | WhitelistedMerchant

  -- inline brittany config for width
  -- brittany-next-binding --columns 500
instance Semigroup MerchantRisk where
  WhitelistedMerchant           <> _                             = WhitelistedMerchant
  MerchantRisk _ WhitelistAllow <> _                             = WhitelistedMerchant
  _                             <> WhitelistedMerchant           = WhitelistedMerchant
  _                             <> MerchantRisk _ WhitelistAllow = WhitelistedMerchant
  MerchantRisk rule VeryHigh    <> _                             = MerchantRisk rule VeryHigh
  MerchantRisk rule High        <> _                             = MerchantRisk rule High
  _                             <> MerchantRisk rule VeryHigh    = MerchantRisk rule VeryHigh
  _                             <> MerchantRisk rule High        = MerchantRisk rule High

  MerchantRisk _ Medium         <> MerchantRisk _    Medium      = MerchantRisk CombinedRiskRules High
  x                             <> NoMerchantRisk                = x
  _                             <> x                             = x

riskyMerchant :: Transaction -> Maybe RiskRules
riskyMerchant Transaction {..} =
  let riskyDescription =
        MerchantRisk RiskyMerchantName . nameRiskScore <$> trxDescription
      riskyName =
        MerchantRisk RiskyMerchantName . nameRiskScore . cmiName <$> trxMerchant
      riskyMCC =
        MerchantRisk RiskyMerchantMCC . mccRiskScore . cmiMcc <$> trxMerchant
      isp2ptrx =
        MerchantRisk RiskyP2P
          .   (\x -> if x == MastercardMCC "4829" then VeryHigh else None)
          .   cmiMcc
          <$> trxMerchant
      isNonUSAmerchant = case cmiCountry <$> trxMerchant of
        Nothing    -> Nothing
        Just "USA" -> Nothing
        Just ""    -> Nothing
        Just "RUS" -> Just $ MerchantRisk RiskyMerchantCountry VeryHigh
        Just "MLT" -> Just $ MerchantRisk RiskyMerchantCountry VeryHigh
        Just "CYP" -> Just $ MerchantRisk RiskyMerchantCountry VeryHigh
        Just "EST" -> Just $ MerchantRisk RiskyMerchantCountry VeryHigh
        Just "PAK" -> Just $ MerchantRisk RiskyMerchantCountry VeryHigh
        Just _     -> Just $ MerchantRisk RiskyMerchantCountry High
  in  riskFolder
        [isp2ptrx, riskyDescription, riskyName, riskyMCC, isNonUSAmerchant]

riskFolder :: [Maybe MerchantRisk] -> Maybe RiskRules
riskFolder riskResults =
  let reducer Nothing  NoMerchantRisk = NoMerchantRisk
      reducer Nothing  risk           = risk
      reducer (Just r) accum          = r <> accum

      folded = foldr reducer NoMerchantRisk riskResults
  in  case folded of
        NoMerchantRisk                   -> Nothing
        WhitelistedMerchant              -> Nothing
        MerchantRisk rule VeryHigh       -> Just rule
        MerchantRisk rule High           -> Just rule
        MerchantRisk _    Medium         -> Nothing
        MerchantRisk _    Low            -> Nothing
        MerchantRisk _    None           -> Nothing
        MerchantRisk _    WhitelistAllow -> Nothing
