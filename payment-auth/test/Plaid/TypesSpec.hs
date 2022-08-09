module Plaid.TypesSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile )
import           Data.Aeson
import           Data.ByteString.Lazy           ( readFile )
import           Data.Ratio
import           Shared.Models.Currency
import           Shared.Models.Plaid.Base
import           Shared.TgthrMessages.Base
import           Test.Hspec
spec :: Spec
spec = parallel $ do
  describe "Exchange token request" $ it "Encodes" $ do
    plaidjson <- readFile "test/json/exchange_token_response.json"
    eitherDecode plaidjson
      `shouldBe` (Right $ ExhangeTokenResponse
                   (AccessToken
                     "access-sandbox-a"
                   )
                   (ItemId "a")
                   "Aim3b"
                 )

  describe "Exchange token request" $ it "Encodes" $ do
    plaidjson <- readFile "test/json/retrieve_auth_response.json"
    eitherDecode plaidjson
      `shouldBe` (Right $ AuthResponse
                   [ Account
                       (PlaidAccountId "a")
                       (Depository Checking)
                       (Currency "USD" (110 % 1))
                       (Just $ Currency "USD" (100 % 1))
                       Nothing
                       (Just "Plaid Checking")
                       (Just "Plaid Gold Checking")
                   ]
                   (Numbers
                     [ ACH
                         (PlaidAccountId "a"
                         )
                         "00000"
                         "00000"
                         (Just "00000")
                     ]
                   )
                   (Item (ItemId "a")
                         "ins_109508"
                         (Just "https://plaid.com/example/hook")
                         Nothing
                         ["balance", "auth"]
                         ["identity", "transactions"]
                   )
                   "m8MDnv9okwxFNBV"
                 )
