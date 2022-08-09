{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Reduce duplication" -}


module Shared.TgthrMessages.AccountsSpec
  ( spec
  ) where

import qualified Data.Aeson                    as A
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy           ( filter
                                                , readFile
                                                )
import qualified Data.UUID                     as U
import           Prelude                 hiding ( filter
                                                , readFile
                                                )
import           Shared.Messages
import           Shared.Models.User
import           Shared.TgthrMessages.Accounts
import           Test.Hspec
import           Utils


spec :: Spec
spec = parallel $ do
  describe "GetUser" $ do
    let g = GetUser { gucUser = UserID U.nil }
    it "encode -> decodes" $ do
      (A.fromJSON . A.toJSON) g `shouldBe` A.Success g
      (A.eitherDecode . A.encode) g `shouldBe` Right g

    it "encodes into kown TgthrMessage" $ do
      let msg = fullMsg { tgthrBody = CommandV1 $ AcctCmd g }
      someJson <- filter (\x -> x /= 32 && x /= 10 && x /= 13)
        <$> readFile "test/json/messages/getuser.json"
      encodePretty' prettyConfig msg `shouldBe` someJson

  describe "GetPasswordReply" $ do
    let r = GetPasswordReply { gprEmail    = EmailAddress "hi@.com"
                             , gprUser     = UserID U.nil
                             , gprPassword = Password "pass"
                             }
    it "encode -> decodes" $ do
      (A.fromJSON . A.toJSON) r `shouldBe` A.Success r
      (A.eitherDecode . A.encode) r `shouldBe` Right r

    it "encodes into kown TgthrMessage" $ do
      let
        msg =
          fullMsg { tgthrBody = ReplyV1 $ ReplySuccessV1 $ AcctReplySuccess r }
      someJson <- filter (\x -> x /= 32 && x /= 10 && x /= 13)
        <$> readFile "test/json/messages/getpasswordreply.json"
      encodePretty' prettyConfig msg `shouldBe` someJson

  describe "GetPasswordReplyFailure" $ do
    let r = GetPasswordReplyFailure { gpfEmail  = EmailAddress "hi@.com"
                                    , gpfReason = PasswordMismatch
                                    }
    it "encode -> decodes" $ do
      (A.fromJSON . A.toJSON) r `shouldBe` A.Success r
      (A.eitherDecode . A.encode) r `shouldBe` Right r

    it "encodes into kown TgthrMessage" $ do
      let msg =
            fullMsg { tgthrBody = ReplyV1 $ ReplyFailureV1 $ AcctFailure r }
      someJson <- filter (\x -> x /= 32 && x /= 10 && x /= 13)
        <$> readFile "test/json/messages/getpasswordreplyfailure.json"
      encodePretty' prettyConfig msg `shouldBe` someJson

  describe "UserWasUpdated" $ do
    let r = UserWasUpdated { uueUser    = UserID U.nil
                           , uueState   = UserWaitingOnKYC
                           , uueChanges = [UsersPhone]
                           }

    it "encode -> decodes" $ do
      (A.fromJSON . A.toJSON) r `shouldBe` A.Success r
      (A.eitherDecode . A.encode) r `shouldBe` Right r

    it "encodes into kown TgthrMessage" $ do
      let msg = fullMsg { tgthrBody = EventV1 $ AccountsEvt r }
      someJson <- filter (\x -> x /= 32 && x /= 10 && x /= 13)
        <$> readFile "test/json/messages/userupdated.json"
      encodePretty' prettyConfig msg `shouldBe` someJson

