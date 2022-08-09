{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module Handlers.LoginSpec
  ( spec
  ) where

import           Data.ByteString                ( ByteString )
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                )
import           Data.List                      ( find )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Handlers.Login     ( signupHandler )
import           LandingPage.Types              ( ClusterEnvironment(..)
                                                , SessionData
                                                )
import           LandingPage.Web                ( setUUIDCookie )
import           Network.Wai.Test               ( SResponse
                                                , simpleHeaders
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldReturn
                                                , xit
                                                )
import           Test.Hspec.Wai                 ( ResponseMatcher(matchStatus)
                                                , liftIO
                                                , postHtmlForm
                                                , request
                                                , shouldRespondWith
                                                , with
                                                )
import qualified Web.ClientSession             as WCS
import           Web.Cookie                     ( parseCookies )
import           Web.Scotty                    as Scotty
                                                ( middleware
                                                , scottyApp
                                                )
import qualified Web.Scotty                    as Scotty

overwriteCount :: IORef Int
{-# NOINLINE overwriteCount #-}
overwriteCount = unsafePerformIO $ newIORef 0

createCount :: IORef Int
{-# NOINLINE createCount #-}
createCount = unsafePerformIO $ newIORef 0

aSessKey :: WCS.Key
{-# NOINLINE aSessKey #-}
aSessKey = unsafePerformIO WCS.getDefaultKey

aSKey :: V.Key SessionData
{-# NOINLINE aSKey #-}
aSKey = unsafePerformIO V.newKey

getSessCookie :: SResponse -> Maybe ByteString
getSessCookie resp = lookup "session" $ maybe [] (parseCookies . snd) $ find
  (\h -> fst h == "Set-Cookie")
  (simpleHeaders resp)

spec :: Spec
spec = do
  let withApp = with . scottyApp

  describe "signupHandler" $ do
    withApp
        (middleware (setUUIDCookie DevelopmentEnv aSessKey aSKey) >> Scotty.post
          "/signup"
          (signupHandler DevelopmentEnv aSessKey aSKey undefined)
        )
      $ do
          it "rejects missing email - text" $ do
            crtCount <- liftIO $ readIORef createCount
            resp     <- postHtmlForm "/signup" []
            let cookie = fromMaybe (error "cookie") (getSessCookie resp)
            request
                "POST"
                "/signup"
                [ ("Cookie"      , cookie)
                , ("Content-Type", "application/x-www-form-urlencoded")
                ]
                ""
              `shouldRespondWith` "Malformed: Param: email not found!"
                                    { matchStatus = 400
                                    }
            liftIO $ readIORef createCount `shouldReturn` crtCount

          it "rejects missing email - json" $ do
            crtCount <- liftIO $ readIORef createCount
            resp     <- postHtmlForm "/signup" []
            let cookie = fromMaybe (error "cookie") (getSessCookie resp)
            request
                "POST"
                "/signup"
                [ ("Cookie"      , cookie)
                , ("Content-Type", "application/x-www-form-urlencoded")
                , ("Accept"      , "application/json")
                ]
                ""
              `shouldRespondWith` "{\"error\":\"Param: email not found!\"}"
                                    { matchStatus = 400
                                    }
            liftIO $ readIORef createCount `shouldReturn` crtCount

          it "rejects missing password" $ do
            crtCount <- liftIO $ readIORef createCount
            resp     <- postHtmlForm "/signup" []
            let cookie = fromMaybe (error "cookie") (getSessCookie resp)
            request
                "POST"
                "/signup"
                [ ("Cookie"      , cookie)
                , ("Content-Type", "application/x-www-form-urlencoded")
                , ("Accept"      , "application/json")
                ]
                "email=a@example.com"
              `shouldRespondWith` "{\"error\":\"Param: password not found!\"}"
                                    { matchStatus = 400
                                    }
            liftIO $ readIORef createCount `shouldReturn` crtCount

          it "rejects bad email" $ do
            crtCount <- liftIO $ readIORef createCount
            resp     <- postHtmlForm "/signup" []
            let cookie = fromMaybe (error "cookie") (getSessCookie resp)
            request
                "POST"
                "/signup"
                [ ("Cookie"      , cookie)
                , ("Content-Type", "application/x-www-form-urlencoded")
                , ("Accept"      , "application/json")
                ]
                "email=a@&password=haxor"
              `shouldRespondWith` "{\"bademail\":true,\"useremail\":\"a@\"}"
                                    { matchStatus = 400
                                    }
            liftIO $ readIORef createCount `shouldReturn` crtCount

          it "rejects simple passwords" $ do
            crtCount <- liftIO $ readIORef createCount
            resp     <- postHtmlForm "/signup" []
            let cookie = fromMaybe (error "cookie") (getSessCookie resp)
            request
                "POST"
                "/signup"
                [ ("Cookie"      , cookie)
                , ("Content-Type", "application/x-www-form-urlencoded")
                , ("Accept"      , "application/json")
                ]
                "email=a@example.com&password=haxor"
              `shouldRespondWith` "{\"passwordrules\":true,\"useremail\":\"a@example.com\"}"
                                    { matchStatus = 400
                                    }
            liftIO $ readIORef createCount `shouldReturn` crtCount

          xit "accepts good info" $ do
            ovrCount <- liftIO $ readIORef overwriteCount
            crtCount <- liftIO $ readIORef createCount
            resp     <- postHtmlForm "/signup" []
            let cookie = fromMaybe (error "cookie") (getSessCookie resp)
            request
                "POST"
                "/signup"
                [ ("Cookie"      , cookie)
                , ("Content-Type", "application/x-www-form-urlencoded")
                , ("Accept"      , "application/json")
                ]
                "email=a@example.com&password=haxor4life"
              `shouldRespondWith` 200
            liftIO $ readIORef overwriteCount `shouldReturn` ovrCount
            liftIO $ readIORef createCount `shouldReturn` crtCount + 1
