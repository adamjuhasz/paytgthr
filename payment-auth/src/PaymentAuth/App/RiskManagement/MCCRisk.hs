module PaymentAuth.App.RiskManagement.MCCRisk where

import           PaymentAuth.App.RiskManagement.Types
                                                ( MerchantRiskScore(..) )
import           Shared.Models.Transaction      ( MastercardMCC(..) )

mccRiskScore :: MastercardMCC -> MerchantRiskScore
mccRiskScore (MastercardMCC "4829") = VeryHigh -- Money Transfer
mccRiskScore (MastercardMCC "5734") = Medium   -- Computer Stores
mccRiskScore (MastercardMCC "5816") = Medium   -- Digital Goods
mccRiskScore (MastercardMCC "5817") = Medium   -- Digital Goods
mccRiskScore (MastercardMCC "5818") = Medium   -- Digital Goods
mccRiskScore (MastercardMCC "5933") = Medium   -- Pawn Shops
mccRiskScore (MastercardMCC "5967") = High     -- Direct Marketing
mccRiskScore (MastercardMCC "5968") = Medium   -- Direct Marketing
mccRiskScore (MastercardMCC "5999") = Medium   -- Misc Store
mccRiskScore (MastercardMCC "6010") = VeryHigh -- Manual Cash Disbursements
mccRiskScore (MastercardMCC "6011") = VeryHigh -- Automated Cash disbursements
mccRiskScore (MastercardMCC "6012") = High     --  Customer Financial Instution
mccRiskScore (MastercardMCC "6050") = VeryHigh -- Quasi Cash
mccRiskScore (MastercardMCC "6051") = VeryHigh -- Quasi Cash
mccRiskScore (MastercardMCC "6211") = VeryHigh -- Securities
mccRiskScore (MastercardMCC "6532") = VeryHigh -- Payment Transaction
mccRiskScore (MastercardMCC "6533") = VeryHigh -- Payment Transaction
mccRiskScore (MastercardMCC "6536") = VeryHigh -- MoneySend (Incoming)
mccRiskScore (MastercardMCC "6537") = VeryHigh --  MoneySend
mccRiskScore (MastercardMCC "6538") = VeryHigh -- MoneySend
mccRiskScore (MastercardMCC "6540") = VeryHigh -- POI Funding
mccRiskScore (MastercardMCC "7273") = High     -- Dating/Escort Services
mccRiskScore (MastercardMCC "7297") = Medium   -- Massage Parlors
mccRiskScore (MastercardMCC "7800") = VeryHigh -- Lottery
mccRiskScore (MastercardMCC "7801") = VeryHigh -- Gambling
mccRiskScore (MastercardMCC "7802") = VeryHigh -- Animal racing
mccRiskScore (MastercardMCC "7994") = High     -- Usually is Gambling
mccRiskScore (MastercardMCC "7995") = VeryHigh -- Gambling
mccRiskScore (MastercardMCC "9223") = VeryHigh -- Bail Bonds
mccRiskScore (MastercardMCC "9311") = VeryHigh -- Tax Paymnets
mccRiskScore (MastercardMCC "9406") = VeryHigh -- Lottery
mccRiskScore _                      = None
