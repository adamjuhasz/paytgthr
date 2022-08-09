{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Reduce duplication" -}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module LandingPage.Web
  ( startServer
  , createServer
  , AppSettings(..)
  , setUUIDCookie
  ) where

import           Control.Concurrent
import           Data.Aeson                     ( KeyValue((.=))
                                                , Object
                                                , decode
                                                , encode
                                                , object
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Foldable                  ( asum )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( isPrefixOf )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.UUID.V4                  as U
import qualified Data.Vault.Lazy               as V
import           LandingPage.Flows.Application  ( appFlow )
import           LandingPage.Flows.Debug        ( debugFlow )
import           LandingPage.Flows.Signup       ( signUpFlow )
import           LandingPage.Flows.Tracking     ( trackingFlow )
import           LandingPage.Types              ( AppSettings(..)
                                                , ClusterEnvironment
                                                  ( ProductionEnv
                                                  )
                                                , SessionData(..)
                                                )
import           LandingPage.Utils              ( AcceptType(..)
                                                , createPublisher
                                                , genAuthToken
                                                , genSessionHeader
                                                , getAcceptType
                                                )
import           Network.Connection             ( TLSSettings(..) )
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Network.HTTP.Types             ( Header
                                                , RequestHeaders
                                                , status503
                                                )
import           Network.HTTP.Types.Status      ( status404
                                                , status500
                                                )
import           Network.Wai                    ( Application
                                                , Middleware
                                                , Request(..)
                                                , Response
                                                , mapResponseHeaders
                                                )
import           Network.Wai.Handler.Warp       ( getPort
                                                , runSettings
                                                , setPort
                                                )
import           Network.Wai.Middleware.AddHeaders
                                                ( addHeaders )
import           Network.Wai.Middleware.Cors    ( CorsResourcePolicy(..)
                                                , cors
                                                , simpleCorsResourcePolicy
                                                )
import           Network.Wai.Middleware.Gzip    ( GzipFiles(GzipCompress)
                                                , GzipSettings
                                                  ( gzipCheckMime
                                                  , gzipFiles
                                                  )
                                                , def
                                                , defaultCheckMime
                                                , gzip
                                                )
import           Network.Wai.Middleware.HttpAuth
                                                ( extractBearerAuth )
import           Network.Wai.Middleware.RequestLogger
                                                ( OutputFormat
                                                  ( CustomOutputFormatWithDetailsAndHeaders
                                                  )
                                                , RequestLoggerSettings
                                                  ( outputFormat
                                                  )
                                                , mkRequestLogger
                                                )
import           Network.Wai.Middleware.Static  ( (>->)
                                                , CachingStrategy
                                                  ( CustomCaching
                                                  , NoCaching
                                                  )
                                                , FileMeta
                                                  ( fm_etag
                                                  , fm_fileName
                                                  , fm_lastModified
                                                  )
                                                , addBase
                                                , initCaching
                                                , noDots
                                                , staticPolicy'
                                                )
import           Servant.Client                 ( mkClientEnv
                                                , parseBaseUrl
                                                )
import           Shared.Track.Stackdriver       ( stackDriverMiddleware )
import           Shared.Web.RequestLogger       ( formatAsJSONWithHeaders )
import           Shared.Web.Utils               ( warpDefaultSettings )
import           System.Directory               ( doesFileExist )
import           Text.Mustache                  ( Template(templateActual)
                                                , compileMustacheDir
                                                , renderMustacheW
                                                )
import           Text.Pretty.Simple             ( pPrint )
import qualified Web.ClientSession             as WCS
import           Web.Scotty                    as Scotty
                                                ( defaultHandler
                                                , file
                                                , get
                                                , html
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , middleware
                                                , next
                                                , notFound
                                                , param
                                                , params
                                                , post
                                                , redirect
                                                , regex
                                                , request
                                                , scottyApp
                                                , setHeader
                                                , status
                                                , text
                                                )

insertIfMissing :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
insertIfMissing h@(hName, _) hs =
  let matchingNames = fmap fst hs & filter (== hName)
  in  case matchingNames of
        [] -> h : hs
        _  -> hs

xorResponseHeader :: Header -> Response -> Response
xorResponseHeader h = mapResponseHeaders $ insertIfMissing h

getSessionData :: WCS.Key -> Maybe B.ByteString -> IO SessionData
getSessionData key sessionCookie = do
  let cookie = sessionCookie >>= WCS.decrypt key <&> BL8.fromStrict
  case cookie >>= decode of
    Just s  -> return s
    Nothing -> do
      newClientID <- U.nextRandom
      return $ SessionData { userID    = Nothing
                           , clientID  = newClientID
                           , invitedBy = Nothing
                           }

setUUIDCookie
  :: ClusterEnvironment -> WCS.Key -> V.Key SessionData -> Middleware
setUUIDCookie env key sKey app req respond = do
  let path     = pathInfo req
  let noCookie = app req respond
  case path of
    []         -> noCookie
    "web"  : _ -> noCookie
    "blog" : _ -> noCookie
    _          -> do
      sessionData <- getSessionData key token
      iv          <- WCS.randomIV
      let vault' = vault req & V.insert sKey sessionData
          req'   = req { vault = vault' }

      let authToken = genAuthToken key iv sessionData
      app req' $ respond . xorResponseHeader
        ("Set-Cookie", genSessionHeader env authToken & TE.encodeUtf8)
 where
  currentHeaders = Network.Wai.requestHeaders req
  sessionHeader  = lookup "Authorization" currentHeaders >>= extractBearerAuth
  token          = asum [sessionHeader]

createServer :: AppSettings -> IO Application
createServer appSettings@AppSettings {..} = do
  sKey      <- V.newKey
  mKey      <- V.newKey
  templates <- compileMustacheDir "error500.html" templateDir

  let staticCachingHeaders :: FileMeta -> RequestHeaders
      staticCachingHeaders fm
        | "static/" `isPrefixOf` fm_fileName fm
        = [ ("Cache-Control", "public, max-age=31536000, immutable")
          , ("Last-Modified", fm_lastModified fm)
          , ("ETag"         , fm_etag fm)
          , ("Vary"         , "Accept-Encoding")
          ]
        | otherwise
        = [ ("Cache-Control", "public,max-age=301")
          , ("Last-Modified", fm_lastModified fm)
          , ("ETag"         , fm_etag fm)
          , ("Vary"         , "Accept-Encoding")
          ]
  cacheContainer <- initCaching $ if environment == ProductionEnv
    then CustomCaching staticCachingHeaders
    else NoCaching

  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = True
        , settingDisableSession               = False
        , settingUseServerName                = False
        }
  internalManager <- newManager $ mkManagerSettings tlsSettings Nothing
  accountsEnv     <- mkClientEnv internalManager
    <$> parseBaseUrl "https://accounts-fsm-web.default.svc.cluster.local:443"
  privacyEnv <- mkClientEnv internalManager <$> parseBaseUrl
    "https://api-privacy-web-internal.default.svc.cluster.local:443"
  paymentAuthEnv <- mkClientEnv internalManager <$> parseBaseUrl
    "https://payment-auth-web-internal.default.svc.cluster.local:443"

  scottyApp $ do
    -- only cathes exceptions in app code not middleware, must be first
    defaultHandler
      (\e -> do
        liftAndCatchIO $ putStr "Error: 500 handler reached" >> pPrint e
        status status500
        accepts <- getAcceptType
        case accepts of
          ApplicationJSON -> Scotty.json (object ["error" .= show e])
          TextHTML        -> do
            let template = renderMustacheW
                  (templates { templateActual = "error500.html" })
                  (object [])
            html (snd template)
      )

    middleware $ setUUIDCookie environment sessionKey sKey
    case stackDriverInfo of
      Nothing -> return ()
      Just x0 -> middleware $ stackDriverMiddleware x0

    middleware . cors . const . Just $ simpleCorsResourcePolicy
      { corsOrigins        = Just
                               ( [ "https://paytgthr.com"
                                 , "https://paytgthr.dev"
                                 , "http://localhost:19006"
                                 , "https://localhost:19006"
                                 , "http://localhost"
                                 , "https://localhost"
                                 , "null"
                                 ]
                               , True
                               )
      , corsRequestHeaders = ["Authorization", "content-type"]
      , corsExposedHeaders = Just
        ["token", "via", "server", "content-type", "content-length", "date"]
      , corsMaxAge         = Just (60 * 5) -- 5 minutes
      , corsRequireOrigin  = False
      , corsMethods        = ["GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS"]
      }

    let mimeChceck "image/svg+xml" = True
        mimeChceck mime            = defaultCheckMime mime
    middleware . gzip $ def { gzipFiles     = GzipCompress
                            , gzipCheckMime = mimeChceck
                            }

    get "/healthy" $ do
      isShuttingDown <- liftAndCatchIO $ tryReadMVar goingToShutdown
      case isShuttingDown of
        Nothing    -> text "ok"
        Just False -> text "ok"
        Just True  -> do
          liftAndCatchIO
            $ putStrLn "SIGTERM received... returning 503 to health check"
          status status503 >> text "not ok"

    post "/report-violation/:reporter" $ do
      reporter :: Text <- param "reporter"
      report :: Object <- jsonData
      liftAndCatchIO . putStrLn . BL8.unpack . encode $ object
        ["error" .= True, "reporter" .= reporter, "report" .= report]
      text "reported"

    get "/getapp" $ do
      uaM <- request <&> requestHeaderUserAgent
      let ua = uaM <&> TE.decodeUtf8
      case ua of
        Nothing -> redirect "/"
        Just str
          | not . null $ T.breakOnAll "iPhone" str
          -> redirect
            "https://apps.apple.com/us/app/pay-tgthr-couples-debit-card/id1463926588?mt=8"
          | not . null $ T.breakOnAll "Android" str
          -> redirect
            "https://play.google.com/store/apps/details?id=com.paytgthr.alfie&utm_source=landingpage&utm_campaign=hero&pcampaignid=pcampaignidMKT-Other-global-all-co-prtnr-py-PartBadge-Mar2515-1"
          | otherwise
          -> redirect "/"

    -- influencer codes
    get (regex "^/([A-Za-z0-9]*)$") $ do
      let inlfuencerList = ["liza", "bradley"]
      captured :: Text <- param "1"
      let normalized = T.toLower captured
      let isCode     = normalized `elem` inlfuencerList
      if not isCode
        then next
        else do
          uaM <- request <&> requestHeaderUserAgent
          let ua = uaM <&> TE.decodeUtf8
          case ua of
            Nothing -> redirect "/"
            Just str
              | not . null $ T.breakOnAll "iPhone" str
              -> redirect
                "https://apps.apple.com/us/app/pay-tgthr-couples-debit-card/id1463926588?mt=8"
              | not . null $ T.breakOnAll "Android" str
              -> redirect
                "https://play.google.com/store/apps/details?id=com.paytgthr.alfie&utm_source=landingpage&utm_campaign=hero&pcampaignid=pcampaignidMKT-Other-global-all-co-prtnr-py-PartBadge-Mar2515-1"
              | otherwise
              -> redirect "/"

    signUpFlow (createPublisher appSettings mKey)
               sKey
               ssnEncrypter
               pinEncrypter
               accountsEnv
               paymentAuthEnv

    appFlow (createPublisher appSettings mKey)
            sKey
            sessionKey
            pinEncrypter
            environment
            accountsEnv
            privacyEnv
            paymentAuthEnv

    trackingFlow withDBPool accountsEnv

    -- debug
    debugFlow (createPublisher appSettings mKey) sKey

    get (regex "^/app/") $ do
      accepts <- getAcceptType
      case accepts of
        ApplicationJSON -> next
        TextHTML        -> redirect "/getapp"

    get "/" $ do
      setHeader "content-type"  "text/html; charset=UTF-8" -- required for GZIP
      setHeader "cache-control" "public, max-age=86400, stale-if-error=300"
      file (publicDir <> "/homepage.html")

    get "/.well-known/apple-app-site-association" $ do
      setHeader "content-type" "application/json" -- required for GZIP
      file (publicDir <> "/apple-app-site-association.json")

    get "/.well-known/assetlinks.json" $ do
      setHeader "content-type" "application/json" -- required for GZIP
      file (publicDir <> "/assetlinks.json")

    get (regex "^/.*$") $ do
      path :: String <- param "0"
      allParams      <- params
      let fullPath = publicDir <> path <> "/index.html"
      doesExist <- liftAndCatchIO $ doesFileExist fullPath
      liftAndCatchIO $ print (path, allParams)
      case (doesExist, length allParams) of
        (True, 1) -> do
          setHeader "content-type"  "text/html; charset=UTF-8" -- required for GZIP
          setHeader "Cache-Control" "public, max-age=86400, stale-if-error=300"
          setHeader "Regex"         "1"
          file fullPath
        (True, _) -> do
          setHeader "content-type" "text/html; charset=UTF-8" -- required for GZIP
          setHeader "Regex"        "1"
          setHeader "QS"           "1"
          file (publicDir <> "/200.html")
        (False, _) -> next

    middleware $ staticPolicy' cacheContainer (noDots >-> addBase publicDir)

    notFound $ do
      setHeader "content-type" "text/html; charset=UTF-8" -- required for GZIP
      status status404
      file (publicDir <> "/404.html")

startServer :: AppSettings -> IO ()
startServer tgthrSettings = do
  let appSettings = warpDefaultSettings & setPort (port tgthrSettings)

  waiApp      <- createServer tgthrSettings
  jsonLogging <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                       formatAsJSONWithHeaders
    }
  let defaultSrc = "default-src 'self' 'unsafe-inline' "
  let
    scriptSrc
      = "script-src 'self' paytgthr.com 'unsafe-inline' *.google-analytics.com static.cloudflareinsights.com ajax.cloudflare.com cdnjs.cloudflare.com cdn.segment.com *.gstatic.com *.facebook.net *.ads-twitter.com ads-twitter.com *.twitter.com *.googletagmanager.com *.redditstatic.com"
  let
    connectSrc
      = "connect-src 'self' paytgthr.com *.segment.io *.google.com *.doubleclick.net *.googleapis.com *.facebook.com *.google-analytics.com us-street.api.smartystreets.com cdn.segment.com"
  let
    imgSrc
      = "img-src 'self' data: paytgthr.com images.unsplash.com *.facebook.com t.co play-lh.googleusercontent.com www.googletagmanager.com www.google.de www.google.co.uk www.google.pl www.google.nl www.google.ie www.google.ca *.reddit.com"
  let fontSrc = "font-src 'self' data: rsms.me fonts.gstatic.com"
  let
    styleSrc
      = "style-src 'self' 'unsafe-inline' data: paytgthr.com unpkg.com cdn.jsdelivr.net rsms.me"
  let objectSrc = "object-src none;"
  let frameSrc = "frame-src www.youtube-nocookie.com www.youtube.com"
  let addSecurityHeaders = addHeaders
        [ ("X-Frame-Options"          , "DENY")
        , ("X-XSS-Protection"         , "1; report=/report-violation/xss")
        , ("X-Content-Type-Options"   , "nosniff")
        -- setting to 0 will disable HSTS (https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security#how_the_browser_handles_it)
        , ("Strict-Transport-Security", "max-age=3600")
        , ( "Content-Security-Policy"
          , defaultSrc
          <> " ; "
          <> scriptSrc
          <> " ; "
          <> connectSrc
          <> " ; "
          <> imgSrc
          <> " ; "
          <> fontSrc
          <> " ; "
          <> styleSrc
          <> " ; "
          <> frameSrc
          <> " ; "
          <> objectSrc
          <> " ; "
          <> "report-uri /report-violation/csp"
          )
        , ("Referrer-Policy"   , "origin-when-cross-origin")
        , ("Feature-Policy"    , "camera 'self' ;")
        , ("Permissions-Policy", "camera=(self)")
        ]

  putStrLn $ "External API on port " <> show (getPort appSettings)
  runSettings appSettings . jsonLogging . addSecurityHeaders $ waiApp

