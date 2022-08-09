module Shared.MessagesSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile
                                                , filter
                                                )
import           Test.Hspec
import qualified Data.Aeson                    as A
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy           ( readFile
                                                , filter
                                                )
import qualified Data.UUID                     as U
import           Data.Maybe

import           Shared.Messages
import           Shared.TgthrMessages.Accounts
import           Shared.Models.User
import           Shared.TgthrMessages.Base
import           Utils

-- mk :: A.Value -> TgthrMessage
-- mk v = TgthrMessage { tgthrMsgid     = MessageID U.nil
--                     , tgthrParent    = Nothing
--                     , tgthrSource    = Nothing
--                     , tgthrBody      = ReplyV1 $ ReplySuccessV1 CmdSuccess
--                     , tgthrTimestamp = aTime
--                     , tgthrUserid    = Just $ UserID U.nil
--                     }

-- defaultEvent :: MessageBody
-- defaultEvent = ReplyV1 $ ReplySuccessV1 CmdSuccess

-- defaultMsg :: TgthrMessage
-- defaultMsg = mk $ A.object []

spec :: Spec
spec = parallel $ describe "JSON" $ do
  describe "ReplySuccessV1" $ do
    let b    = ReplyV1 $ ReplySuccessV1 CmdSuccess
        mevt = fullMsg { tgthrBody = b }

    it "encodes -> decodes" $ do
      let mevtEither = (A.eitherDecode . A.encode) mevt
      mevtEither `shouldBe` Right mevt
      let mevt' = fromRight mevtEither
      tgthrMsgid mevt' `shouldBe` (MessageID . fromJust $ uuid3s)
      tgthrParent mevt' `shouldBe` MessageID <$> uuid2s
      tgthrSource mevt' `shouldBe` MessageID <$> uuid1s
      tgthrBody mevt' `shouldBe` b
      tgthrTimestamp mevt' `shouldBe` aTime
      tgthrUserid mevt' `shouldBe` (Just $ UserID U.nil)

    it "encodes to known format" $ do
      someJson <- filter (\x -> x /= 32 && x /= 10 && x /= 13)
        <$> readFile "test/json/MessageReplySuccess.json"
      encodePretty' prettyConfig mevt `shouldBe` someJson

  describe "CommandV1" $ do
    let b    = CommandV1 . AcctCmd $ GetUser { gucUser = UserID U.nil }
        mcmd = fullMsg { tgthrBody = b }

    it "encodes -> decodes" $ do
      let mcmdEither = (A.eitherDecode . A.encode) mcmd
      mcmdEither `shouldBe` Right mcmd
      let mcmd' = fromRight mcmdEither
      tgthrMsgid mcmd' `shouldBe` (MessageID . fromJust $ uuid3s)
      tgthrParent mcmd' `shouldBe` MessageID <$> uuid2s
      tgthrSource mcmd' `shouldBe` MessageID <$> uuid1s
      tgthrBody mcmd' `shouldBe` b
      tgthrTimestamp mcmd' `shouldBe` aTime
      tgthrUserid mcmd' `shouldBe` (Just $ UserID U.nil)

    it "encodes to known format" $ do
      someJson <- filter (\x -> x /= 32 && x /= 10 && x /= 13)
        <$> readFile "test/json/messages/getuser.json"
      encodePretty' prettyConfig mcmd `shouldBe` someJson

  describe "EventV1" $ do
    let b = EventV1 . AccountsEvt $ UserWasCreated
          { uceUser   = UserID U.nil
          }
        msucc = fullMsg { tgthrBody = b }

    it "encodes -> decodes" $ do
      let msuccDecided :: Either String TgthrMessage =
            (A.eitherDecode . A.encode) msucc
      msuccDecided `shouldBe` Right msucc
      let msucc' :: TgthrMessage = fromRight msuccDecided
      tgthrMsgid msucc' `shouldBe` (MessageID . fromJust $ uuid3s)
      tgthrParent msucc' `shouldBe` MessageID <$> uuid2s
      tgthrSource msucc' `shouldBe` MessageID <$> uuid1s
      tgthrBody msucc' `shouldBe` b
      tgthrTimestamp msucc' `shouldBe` aTime
      tgthrUserid msucc' `shouldBe` (Just $ UserID U.nil)

  describe "encoding edge-cases" $ it "ignores unknown versions" $ do
    someJson <- readFile "test/json/messages/getuser-v1.1.json"
    (A.decode someJson :: Maybe TgthrMessage) `shouldBe` Nothing

  --     it "ignores incorrect body" $ do
  --       let t =
  --             ""
  --               <> "{"
  --               <> "\"parent\":\"00000000-0000-0000-0000-000000000000\""
  --               <> ",\"body\": []"
  --               <> ",\"version\":\"1.1\""
  --               <> ",\"source\":\"00000000-0000-0000-0000-000000000000\""
  --               <> ",\"id\":\"00000000-0000-0000-0000-000000000000\""
  --               <> ",\"type\":\"reply\""
  --               <> ",\"timestamp\":\"2019-01-01T00:00:00Z\""
  --               <> "}"
  --       (A.decode t :: Maybe TgthrMessage) `shouldBe` Nothing

  --   describe "ReplyFailureV1" $ do
  --     let b     = ReplyFailureV1 { falError = A.object [("key", "city")] }
  --         mfail = fullMsg { body = b }

  --     it "encodes -> decodes" $ do
  --       let m' = (fromJust . A.decode . A.encode) mfail
  --       m' `shouldBe` mfail
  --       msgid m' `shouldBe` (MessageID . fromJust $ uuid3s)
  --       parent m' `shouldBe` MessageID <$> uuid2s
  --       source m' `shouldBe` MessageID <$> uuid1s
  --       body m' `shouldBe` b
  --       timestamp m' `shouldBe` aTime

  --     it "encodes to known format" $ do
  --       let t =
  --             ""
  --               <> "{"
  --               <> "\"parent\":\"00000000-0000-0000-0000-000000000000\""
  --               <> ",\"body\":"
  --               <> "{"
  --               <> "\"key\":\"city\""
  --               <> "}"
  --               <> ",\"user\":\"00000000-0000-0000-0000-000000000000\""
  --               <> ",\"version\":\"1.0\""
  --               <> ",\"source\":\"00000000-0000-0000-0000-000000000000\""
  --               <> ",\"id\":\"00000000-0000-0000-0000-000000000000\""
  --               <> ",\"type\":\"replyerror\""
  --               <> ",\"timestamp\":\"2019-01-01T00:00:00Z\""
  --               <> "}"
  --       A.encode mfail `shouldBe` t

  -- describe "createTgthrMsg" $ do
  --   let gpuuid = fromJust $ U.fromString "00000000-0000-0000-0000-000000000000"
  --       puuid  = fromJust $ U.fromString "00000000-0000-0000-0000-000000000000"
  --       muuid  = fromJust $ U.fromString "00000000-0000-0000-0000-000000000000"

  --   it "accepts no parent" $ do
  --     let m = createTgthrMsg (U.nil, aTime) Nothing defaultEvent
  --     parent m `shouldBe` Nothing
  --   it "accepts parent with no grandparent" $ do
  --     let
  --       p = defaultMsg { msgid  = MessageID puuid
  --                      , parent = Nothing
  --                      , source = Nothing
  --                      }
  --       m = createTgthrMsg (muuid, aTime) (Just p) defaultEvent
  --     msgid m `shouldSatisfy` (== MessageID muuid)
  --     parent m `shouldSatisfy` ((== MessageID puuid) . fromJust)
  --     source m `shouldSatisfy` ((== MessageID puuid) . fromJust)

  --   it "accepts parent with grandparent" $ do
  --     let p = defaultMsg { msgid  = MessageID puuid
  --                        , parent = Just . MessageID $ gpuuid
  --                        , source = Just . MessageID $ gpuuid
  --                        }
  --         m = createTgthrMsg (muuid, aTime) (Just p) defaultEvent
  --     msgid m `shouldSatisfy` (== MessageID muuid)
  --     parent m `shouldSatisfy` ((== MessageID puuid) . fromJust)
  --     source m `shouldSatisfy` ((== MessageID gpuuid) . fromJust)

  -- describe "valueFromMsg + valueFromBody" $ do
  --   it "EventV1" $ do
  --     let b =
  --           EventV1 { evtName = "test", evtValue = A.object [("key", "city")] }
  --         m = fullMsg { body = b }
  --     valueFromMsg m `shouldBe` A.object [("key", "city")]
  --   it "CommandV1" $ do
  --     let
  --       b = CommandV1 { cmdName  = "test-do"
  --                     , cmdValue = A.object [("key", "city")]
  --                     }
  --       m = fullMsg { body = b }
  --     valueFromMsg m `shouldBe` A.object [("key", "city")]
  --   it "ReplySuccessV1" $ do
  --     let b = ReplySuccessV1 { sucValue = A.object [("key", "city")] }
  --         m = fullMsg { body = b }
  --     valueFromMsg m `shouldBe` A.object [("key", "city")]
  --   it "ReplyFailureV1" $ do
  --     let b = ReplyFailureV1 { falError = A.object [("key", "city")] }
  --         m = fullMsg { body = b }
  --     valueFromMsg m `shouldBe` A.object [("key", "city")]


