{- HLINT ignore "Use lambda-case" -}

module LandingPage.Utils where

import           Data.Aeson                     ( encode
                                                , object
                                                )
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder        ( toLazyByteString )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Duration                  ( getSeconds
                                                , year
                                                )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as TL
import           Data.Time.Clock                ( secondsToDiffTime )
import qualified Data.Vault.Lazy               as V
import           GHC.Stack                      ( HasCallStack )
import           LandingPage.Types              ( AppSettings(..)
                                                , ClusterEnvironment(..)
                                                , SessionData(..)
                                                , UserSignupStep(..)
                                                , sessionCookieName
                                                , stepToPath
                                                )
import           Network.HTTP.Types             ( status401
                                                , status403
                                                )
import           Network.Wai                    ( Request
                                                  ( rawPathInfo
                                                  , requestMethod
                                                  , vault
                                                  )
                                                )
import           Network.Wai.Parse              ( parseHttpAccept )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Amqp                    ( AMQPPublisher
                                                , MessageBody(..)
                                                , TgthrMessage
                                                , failureWithText
                                                , publishWithReply
                                                )
import           Shared.Amqp.Utils              ( getUser )
import           Shared.Messages                ( tgthrBody )
import           Shared.Models.User             ( UserID(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , asClientM
                                                , incrementTrace
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , parseTrace
                                                , randomTrace
                                                )
import qualified Web.ClientSession             as WCS
import           Web.Cookie                     ( SetCookie(..)
                                                , defaultSetCookie
                                                , renderSetCookie
                                                , sameSiteNone
                                                , sameSiteStrict
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , header
                                                , json
                                                , liftAndCatchIO
                                                , redirect
                                                , request
                                                , setHeader
                                                , status
                                                )

createPublisher :: AppSettings -> V.Key TgthrMessage -> ActionM AMQPPublisher
createPublisher appSettings mKey = do
  parentMsg <- request <&> vault <&> V.lookup mKey
  case amqpChannel appSettings of
    Nothing ->
      return (\_ -> return . ReplyV1 $ failureWithText "AMQP disabled")
    Just chan -> return
      (\m -> publishWithReply chan parentMsg (CommandV1 m) <&> fst <&> tgthrBody
      )

expectSession :: HasCallStack => Maybe a -> a
expectSession Nothing  = error "Expected valid session"
expectSession (Just x) = x

requireUser :: SessionData -> ActionM UserID
requireUser session = case session of
  SessionData { userID = Just uid } -> do
    req <- request
    let method = requestMethod req
    let path   = rawPathInfo req
    liftAndCatchIO $ putStr "User accessed " >> print (uid, method, path)
    return uid
  _ -> do
    accepts <- getAcceptType
    case accepts of
      ApplicationJSON -> status status401 >> Scotty.json (object [])
      TextHTML        -> redirect (TL.fromStrict $ stepToPath Login)
    finish

requireUserNotClosed :: AMQPPublisher -> SessionData -> ActionM UserID
requireUserNotClosed pub session = do
  user       <- requireUser session
  userModelE <- liftAndCatchIO $ getUser pub user
  case userModelE of
    Left e -> do
      liftAndCatchIO $ putStr "requireUserNotClosed failed " >> print (user, e)
      status status401 >> Scotty.json (object [("error", "failed")]) >> finish
    Right UserModel { usrUserState = UserClosed r } -> do
      liftAndCatchIO $ putStr "requireUserNotClosed closed " >> print (user, r)
      status status403 >> Scotty.json (object [("error", "closed")]) >> finish
    Right _ -> return user

type AccountsEnv = ClientEnv
requireUserNotClosedHTTP
  :: AccountsEnv -> SessionData -> ActionM (UserID, UserModel)
requireUserNotClosedHTTP accountsEnv session = do
  uid   <- requireUser session
  trace <- createTrace
  let getUserModel = _UserGet asClientM trace uid
  userModelE <- liftAndCatchIO $ runClientM getUserModel accountsEnv
  case userModelE of
    Left e -> do
      liftAndCatchIO $ putStr "Error: _UserGet " >> print (uid, e)
      status status401 >> Scotty.json (object [("error", "failed")]) >> finish
    Right UserModel { usrUserState = UserClosed r } -> do
      liftAndCatchIO $ putStr "requireUserNotClosedHTTP closed " >> print
        (uid, r)
      status status403 >> Scotty.json (object [("error", "closed")]) >> finish
    Right user -> return (uid, user)

genAuthToken :: WCS.Key -> WCS.IV -> SessionData -> BS.ByteString
genAuthToken key iv = WCS.encrypt key iv . BL8.toStrict . encode

genSessionHeader :: ClusterEnvironment -> BS.ByteString -> Text
genSessionHeader env token = cookie & renderSetCookie'
 where
  cookie = defaultSetCookie
    { setCookieName     = sessionCookieName
    , setCookieValue    = token
    , setCookieMaxAge   = 2 * year & getSeconds & secondsToDiffTime & Just
    , setCookieHttpOnly = True
    , setCookiePath     = Just "/"
    , setCookieSecure   = env /= DevelopmentEnv
    , setCookieSameSite = Just $ if env == ProductionEnv
                            then sameSiteStrict
                            else sameSiteNone
    }

renderSetCookie' :: SetCookie -> Text
renderSetCookie' =
  TE.decodeUtf8 . BL.toStrict . toLazyByteString . renderSetCookie

genTokenHeader
  :: ClusterEnvironment -> WCS.Key -> SessionData -> ActionM BS.ByteString
genTokenHeader env wcsKey session = do
  iv <- liftAndCatchIO WCS.randomIV
  let token = genAuthToken wcsKey iv session
  setHeader "Set-Cookie" . TL.fromStrict $ genSessionHeader env token
  setHeader "Token" . TL.fromStrict $ TE.decodeUtf8 token
  return token

data AcceptType
  = TextHTML
  | ApplicationJSON
  deriving (Eq, Show)

getAcceptType :: ActionM AcceptType
getAcceptType =
  header "Accept"
    <&> fromMaybe ""
    <&> (TE.encodeUtf8 . TL.toStrict)
    <&> parseHttpAccept
    <&> listToMaybe
    <&> fromMaybe "text/html"
    <&> (\acceptType -> case acceptType of
          "text/html"        -> TextHTML
          "application/json" -> ApplicationJSON
          _                  -> TextHTML
        )

getSessionUser :: V.Key SessionData -> ActionM UserID
getSessionUser sKey =
  request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser

createTrace :: Scotty.ActionM TraceContext
createTrace = do
  traceMaybe <- Scotty.header "X-Cloud-Trace-Context"
  case traceMaybe of
    Nothing -> randomTrace
    Just t  -> case parseTrace $ TL.toStrict t of
      Left e -> do
        Scotty.liftAndCatchIO $ putStr "Error: X-Cloud-Trace-Context " >> print
          (traceMaybe, e)
        randomTrace
      Right trace -> incrementTrace trace
