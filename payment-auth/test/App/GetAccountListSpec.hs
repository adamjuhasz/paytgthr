{- HLINT ignore "Redundant do" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module App.GetAccountListSpec
  ( spec
  ) where

import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.UUID                     as U
import           PaymentAuth.App.GetAccountList
                                               as GAL
                                                ( GetTheAccountListErrors
                                                  ( NoAccessToken
                                                  )
                                                , getTheAccountList
                                                )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB(..) )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Plaid.Base       ( ACH(..)
                                                , AccessToken(..)
                                                , Account(..)
                                                , AuthResponse
                                                  ( AuthResponse
                                                  , authAccounts
                                                  , authItem
                                                  , authNumbers
                                                  , authRequestId
                                                  )
                                                , Item(..)
                                                , ItemId(ItemId)
                                                , Numbers(Numbers, ach)
                                                )
import           Shared.Models.User             ( UserID(UserID) )
import           Shared.TgthrMessages.Base      ( AccountType(Depository)
                                                , DepositoryType(..)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AccountDetails(AccountDetails)
                                                , PlaidAccountId(PlaidAccountId)
                                                , PlaidEnvironment(..)
                                                )

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldThrow
                                                )

accessToken :: AccessToken
accessToken = AccessToken "AccessToken"

checkingAccount1 :: Account
checkingAccount1 = Account { accountId               = PlaidAccountId "1"
                           , accountType             = Depository Checking
                           , accountCurrentBalance   = Currency "USD" 100
                           , accountAvailableBalance = Nothing
                           , accountLimit            = Nothing
                           , accountName             = Just "Checking 1"
                           , accountOfficialName     = Nothing
                           }

savingsAccount1 :: Account
savingsAccount1 = Account { accountId               = PlaidAccountId "2"
                          , accountType             = Depository Savings
                          , accountCurrentBalance   = Currency "USD" 1000
                          , accountAvailableBalance = Nothing
                          , accountLimit            = Nothing
                          , accountName             = Just "Savings 1"
                          , accountOfficialName     = Nothing
                          }

prepaidAccount1 :: Account
prepaidAccount1 = Account { accountId               = PlaidAccountId "3"
                          , accountType             = Depository Prepaid
                          , accountCurrentBalance   = Currency "USD" 9.56
                          , accountAvailableBalance = Nothing
                          , accountLimit            = Nothing
                          , accountName             = Just "Prepaid 3"
                          , accountOfficialName     = Nothing
                          }

achCheckingInfo :: ACH
achCheckingInfo = ACH { achAccountId   = PlaidAccountId "1"
                      , achDDANumber   = "123"
                      , achABARouting  = "234"
                      , achWireRouting = Nothing
                      }

achSavingsInfo :: ACH
achSavingsInfo = ACH { achAccountId   = PlaidAccountId "2"
                     , achDDANumber   = "987"
                     , achABARouting  = "876"
                     , achWireRouting = Nothing
                     }

genericItem :: Item
genericItem = Item { itemId                = ItemId "xyz"
                   , itemInstitutionId     = "ABV"
                   , itemWebhook           = Nothing
                   , itemError             = Nothing
                   , itemAvailableProducts = []
                   , itemBilledProducts    = []
                   }

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )
instance HasPlaidDB TestMonad where
  getAccessToken u | u == UserID U.nil = return $ Just (accessToken, Sandbox)
                   | otherwise         = error "bad user"
  getPlaidAuth (AccessToken "AccessToken", Sandbox) =
    return . Right $ AuthResponse
      { authAccounts  = [checkingAccount1, savingsAccount1, prepaidAccount1]
      , authNumbers   = Numbers { ach = [achSavingsInfo, achCheckingInfo] }
      , authItem      = genericItem
      , authRequestId = "123"
      }
  getPlaidAuth _ = error "bad token"

newtype NoAccessToken a =
    NoAccessToken { noAccessToken :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )
instance HasPlaidDB NoAccessToken where
  getAccessToken _ = return Nothing

newtype MissingACH a =
    MissingACH { missingACH :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )
instance HasPlaidDB MissingACH where
  getAccessToken _ = return $ Just (accessToken, Sandbox)
  getPlaidAuth _ = return . Right $ AuthResponse
    { authAccounts  = [checkingAccount1, savingsAccount1, prepaidAccount1]
    , authNumbers   = Numbers { ach = [achSavingsInfo] }
    , authItem      = genericItem
    , authRequestId = "123"
    }

spec :: Spec
spec = parallel $ do
  describe "getTheAccountList" $ do
    it "fails gracefully" $ do
      noAccessToken (getTheAccountList (UserID U.nil))
        `shouldThrow` (== GAL.NoAccessToken)

    it "returns both" $ do
      list <- unTestMonad $ getTheAccountList (UserID U.nil)
      list `shouldBe` Right
        [ AccountDetails (PlaidAccountId "1")
                         "Checking 1"
                         "234"
                         "123"
                         (Currency "USD" 100)
                         (Depository Checking)
        , AccountDetails (PlaidAccountId "2")
                         "Savings 1"
                         "876"
                         "987"
                         (Currency "USD" 1000)
                         (Depository Savings)
        ]

    it "doesn't crash missing ACH" $ do
      list <- missingACH $ getTheAccountList (UserID U.nil)
      list `shouldBe` Right
        [ AccountDetails (PlaidAccountId "2")
                         "Savings 1"
                         "876"
                         "987"
                         (Currency "USD" 1000)
                         (Depository Savings)
        ]
