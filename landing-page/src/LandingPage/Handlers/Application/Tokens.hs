{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Use lambda-case" -}

module LandingPage.Handlers.Application.Tokens where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData(..) )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types.Status      ( status400
                                                , status403
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Token            ( ExpirationTime(TenMinutes)
                                                , TokenId(TokenId)
                                                , TokenMedium(..)
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , PhoneNumber(..)
                                                , UserID
                                                , UserModel(usrUserID)
                                                , normalizeEmail
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , TraceContext
                                                , asClientM
                                                , incrementTrace
                                                )
import qualified Web.Scotty                    as Scotty

mediumConvert :: Text -> Scotty.ActionM TokenMedium
mediumConvert "sms"   = return PhoneMedium
mediumConvert "push"  = return PushMedium
mediumConvert "email" = return EmailMedium
mediumConvert m       = do
  Scotty.liftAndCatchIO
    .  putStr
    $  "Error: mediumConvert does not know "
    <> show m
  Scotty.status status400
  Scotty.json (object [])
  Scotty.finish

findUser
  :: ClientEnv
  -> TraceContext
  -> Maybe EmailAddress
  -> Maybe PhoneNumber
  -> IO (Either () UserID)
findUser env trace email phone = do
  trace' <- incrementTrace trace
  usersM <- case (email, phone) of
    (Nothing, Nothing) -> error "nothing to look for"
    (Just e , _      ) -> runClientM (_UsersQueryEmail asClientM trace' e) env
    (Nothing, Just p ) -> runClientM (_UsersQueryPhone asClientM trace' p) env
  case usersM of
    Left  e          -> error $ "Error: findUser " <> show (email, phone, e)
    Right []         -> return $ Left ()
    Right (user : _) -> return . Right $ usrUserID user

-- | Create a token
createToken :: ClientEnv -> Scotty.ActionM ()
createToken env = do
  medium             <- Scotty.param "medium" >>= mediumConvert
  uniqueInfo :: Text <- Scotty.param "identifier"

  let (email, phone) = case medium of
        EmailMedium ->
          (Just . normalizeEmail $ EmailAddress uniqueInfo, Nothing)
        PhoneMedium -> (Nothing, Just $ PhoneNumber uniqueInfo)
        PushMedium  -> error "Error: Push not allowed"

  parentTrace <- createTrace

  trace'      <- incrementTrace parentTrace
  foundUser   <- Scotty.liftAndCatchIO $ findUser env trace' email phone

  uid         <- case foundUser of
    Right u -> return u
    Left _ ->
      Scotty.status status400
        >> Scotty.json (object ["error" .= True])
        >> Scotty.finish

  tokenId <- TokenId <$> Scotty.liftAndCatchIO nextRandom

  trace'' <- incrementTrace parentTrace
  let fn = _TokenCreate asClientM trace'' uid medium TenMinutes tokenId
  result <- Scotty.liftAndCatchIO $ runClientM fn env

  case result of
    Right _ -> Scotty.json (object [])
    Left e ->
      error $ "Error: Failure with _TokenCreate " <> show (uid, e, trace')

verifyAToken
  :: ClientEnv -> TraceContext -> UserID -> TokenMedium -> Text -> IO Bool
verifyAToken env trace user medium code = do
  trace' <- incrementTrace trace
  let fn = _TokenVerify asClientM trace' user medium code
  result <- runClientM fn env

  case result of
    Right res -> return res
    Left  e   -> do
      putStr "Error: Failure with _TokenVerify " >> print (user, e, trace')
      return False

-- | Verify a given token
verifyToken :: V.Key SessionData -> ClientEnv -> Scotty.ActionM ()
verifyToken sKey env = do
  user <-
    Scotty.request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  medium <- Scotty.param "medium" >>= mediumConvert
  code   <- Scotty.param "code"
  trace  <- createTrace

  res    <- Scotty.liftAndCatchIO $ verifyAToken env trace user medium code
  if res
    then Scotty.json (object ["verified" .= True])
    else do
      Scotty.status status403
      Scotty.json (object ["verified" .= False])
