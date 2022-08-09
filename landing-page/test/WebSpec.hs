{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}

module WebSpec
  ( spec
  ) where

import           Control.Concurrent             ( newEmptyMVar )
import           Data.Aeson                     ( decode )
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.CaseInsensitive           ( CI(..) )
import           Data.Coerce                    ( coerce )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import qualified Data.Text.Encoding            as TE
import           Data.UUID                      ( fromText )
import           LandingPage.TokenUtils         ( createToken )
import           LandingPage.Types              ( AppSettings(..)
                                                , AptoSecrets(AptoSecrets)
                                                , ClusterEnvironment(..)
                                                , EncryptedPin(EncryptedPin)
                                                , EncryptedSSN(EncryptedSSN)
                                                , EncryptedToken(EncryptedToken)
                                                , SessionData(..)
                                                , sessionCookieName
                                                )
import           LandingPage.Web                ( createServer )
import           Network.HTTP.Types             ( Header
                                                , methodGet
                                                )
import           Network.Wai.Test               ( SResponse(simpleHeaders) )
import           Shared.Models.User             ( UserID(UserID) )
import           Shared.Vault                   ( PlainText(PlainText) )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldSatisfy
                                                )
import           Test.Hspec.Wai                 ( get
                                                , liftIO
                                                , pendingWith
                                                , request
                                                , shouldRespondWith
                                                , with
                                                )
import           Text.Regex.TDFA                ( (=~) )
import qualified Web.ClientSession             as WCS
import           Web.Cookie                     ( parseCookies )

testSettings :: AppSettings
testSettings = AppSettings
  { environment      = ProductionEnv
  , amqpChannel      = Nothing
  , port             = 3214
  , sessionKey       =
    makeKey
      "QWrZRNnMUB8fV5QKeAma0d83VYNJHiOv2RtQCaZbLxIonMjCK+VRRH8whss9oOxfimVVdS4XvwCw5Bu4J5hL7ZiraH6kvIZ3t+JZssTGW3alDQtNyGJVQJJw/4EZ0HzG"
  , ssnEncrypter     = return . coerce
  , pinEncrypter     = return . coerce
  , plaidEnvironment = "sandbox"
  , tokenEncrypter   = return . coerce
  , tokenDecrypter   = return . coerce
  , publicDir        = "public"
  , templateDir      = "templates"
  , linkGet          = \_ -> return Nothing
  , linkPut          = \_ -> return ""
  , domain           = ""
  , aptoSecrets      = AptoSecrets "" "" ""
  , withDBPool       = const $ error "missing"
  , stackDriverInfo  = Nothing
  , goingToShutdown  = unsafePerformIO newEmptyMVar
  }
 where
  makeKey s = s & BS.pack & B64.decode & fromRight & WCS.initKey & fromRight
  fromRight a = case a of
    Left  _ -> Prelude.error "Left not allowed"
    Right x -> x

matchHeader :: CI BS.ByteString -> String -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ lookup name headers

spec :: Spec
spec = with (createServer testSettings) $ describe "Web Server" $ do
  it "serves" $ get "/" `shouldRespondWith` 200

  it "has a health endpoint" $ get "/healthy" `shouldRespondWith` 200

  it "has correct headers" $ do
    res <- get "/"
    liftIO $ simpleHeaders res `shouldSatisfy` matchHeader
      "Cache-Control"
      "public, max-age=86400, stale-if-error=300"

  it "accepts malformed cookie" $ do
    iv <- liftIO WCS.randomIV
    request
        methodGet
        "/"
        [ ( "Cookie"
          , sessionCookieName
          <> "="
          <> WCS.encrypt (sessionKey testSettings) iv "123"
          <> "; "
          )
        ]
        ""
      `shouldRespondWith` 200

  it "accepts bad encrypted cookie" $ do
    request methodGet "/" [("Cookie", sessionCookieName <> "=123; ")] ""
      `shouldRespondWith` 200

  it "stores invitation" $ do
    Test.Hspec.Wai.pendingWith "need to make a better encrupter"
    (EncryptedToken token) <- liftIO
      $ createToken (return . coerce) "00000000-0000-0000-0000-000000000000"
    let inviter = UserID <$> fromText "00000000-0000-0000-0000-000000000000"
        url     = "/?invite=" <> TE.encodeUtf8 token
    res' <- request methodGet url [] ""
    let cid =
          lookup "Set-Cookie" (simpleHeaders res')
            <&> parseCookies
            >>= lookup sessionCookieName
            >>= WCS.decrypt (sessionKey testSettings)
            <&> BL8.fromStrict
            >>= decode
            >>= invitedBy
    liftIO $ cid `shouldSatisfy` (==) inviter

