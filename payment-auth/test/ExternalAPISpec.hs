module ExternalAPISpec
  ( spec
  ) where

import           Control.Concurrent             ( newEmptyMVar )
import           Data.ByteString.Lazy           ( readFile )
import           Data.Time.Clock.POSIX          ( getCurrentTime )
import           Data.UUID                      ( nil )
import           Data.UUID.V4                   ( nextRandom )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           PaymentAuth.ExternalAPI        ( ApplicationConfig(..)
                                                , Environment(Testing)
                                                , app
                                                )
import           PaymentAuth.Types              ( DBActions(dbGetUserFromItem) )
import           Prelude                 hiding ( readFile )
import           Scaffolding.Mocks              ( mockDBActions )
import           Shared.Messages                ( MessageBody
                                                , TgthrMessage
                                                , createTgthrMsg
                                                )
import           Shared.Models.User             ( UserID(UserID) )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Wai                 ( ResponseMatcher(matchStatus)
                                                , get
                                                , liftIO
                                                , post
                                                , shouldRespondWith
                                                , with
                                                )


fakeAMQP :: Maybe TgthrMessage -> MessageBody -> IO TgthrMessage
fakeAMQP p m = do
  requestID <- nextRandom
  now       <- getCurrentTime
  print m
  return $ createTgthrMsg (requestID, now) p m

testConfig :: ApplicationConfig
testConfig = ApplicationConfig
  { sendMessage    = fakeAMQP
  , environment    = Testing
  , db = mockDBActions { dbGetUserFromItem = \_ -> return . Just . UserID $ nil
                       }
  , isShuttingDown = unsafePerformIO newEmptyMVar
  }

spec :: Spec
spec = with (app testConfig) $ do
  describe "GET /ping" $ do
    it "responds with 200" $ get "/ping" `shouldRespondWith` 200

    it "responds with 'pong'" $ get "/ping" `shouldRespondWith` "pong"
      { matchStatus = 200
      }

  describe "POST /webhook" $ do
    it "Rejects Malformed" $ post "/webhook" "{}" `shouldRespondWith` 400

    it "Accepts Unknown"
      $                   post "/webhook" "{\"webhook_type\":\"new\"}"
      `shouldRespondWith` "Unknown" { matchStatus = 200 }

    it "AUTH" $ do
      jsoncontents <- liftIO $ readFile "test/json/DEFAULT_UPDATE.json"
      post "/webhook" jsoncontents
        `shouldRespondWith` "Transactions" { matchStatus = 200 }
