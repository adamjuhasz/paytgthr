{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Callback.AuthorizeTransaction.FindSplitForTransactionSpec
  ( spec
  ) where

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( nil )
import           PaymentAuth.App.Purchases.AuthorizeTransaction
                                                ( findSplitForTransaction
                                                , matchCategories
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Scaffolding.Categories         ( cat000
                                                , cat001
                                                )
import           Scaffolding.Groups             ( basicGroup )
import           Scaffolding.Transactions       ( basicTrx )
import           Shared.Models.CategorySplit    ( CategoryCode(..)
                                                , CategorySplit(splits, state)
                                                , CategoryState(..)
                                                )
import           Shared.Models.Group            ( GroupModel(grpId, grpRevision)
                                                , GroupSplit(GroupSplit)
                                                )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.WebAPI.General.API      ( midToTrace )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

instance HasAccounts TestMonad where
  getGroupFor _ _ = return $ Just basicGroup
  getCategorySplits _ _ = return [cat000, cat001]
instance HasTime TestMonad where
  getCurrentTime = liftIO Clock.getCurrentTime

theUser :: UserID
theUser = trxUserId basicTrx

spec :: Spec
spec = parallel $ do
  describe "findSplitForTransaction" $ do
    it "uses default cat for non-categorizable" $ do
      let nonCattableTrx = basicTrx
      trace <- midToTrace (MessageID nil)
      res <- unTestMonad $ findSplitForTransaction trace nonCattableTrx theUser
      res
        `shouldBe` ( Just (grpId basicGroup, grpRevision basicGroup)
                   , (\(GroupSplit u s _) -> (u, s)) <$> splits cat000
                   )

    it "uses correct cat for categorizable" $ do
      let cattableTrx = basicTrx
            { trxMerchant = Just
                              (CardMerchant { cmiMcc      = MastercardMCC "4121"
                                            , cmiMccDesc  = ""
                                            , cmiName     = ""
                                            , cmiLocality = Just ""
                                            , cmiRegion   = Nothing
                                            , cmiCountry  = ""
                                            }
                              )
            }
      trace <- midToTrace (MessageID nil)
      res   <- unTestMonad $ findSplitForTransaction trace cattableTrx theUser
      res
        `shouldBe` ( Just (grpId basicGroup, grpRevision basicGroup)
                   , (\(GroupSplit u s _) -> (u, s)) <$> splits cat001
                   )

  describe "matchCategories" $ do
    it "sorts cats correctly" $ do
      matchCategories
          [cat000 { state = CategoryActive }, cat001 { state = CategoryActive }]
          [(Category000, True), (Category001, True)]
        `shouldBe` [ cat001 { state = CategoryActive }
                   , cat000 { state = CategoryActive }
                   ]

    it "ignores disabled cats" $ do
      matchCategories
          [ cat000 { state = CategoryActive }
          , cat001 { state = CategoryDisabled }
          ]
          [(Category000, True), (Category001, True)]
        `shouldBe` [cat000 { state = CategoryActive }]

    it "ignored non mathing cats" $ do
      matchCategories
          [cat000 { state = CategoryActive }, cat001 { state = CategoryActive }]
          [(Category000, True), (Category001, False)]
        `shouldBe` [cat000 { state = CategoryActive }]
