{- HLINT ignore "Use lambda-case" -}

module LandingPage.Handlers.Login.ForgotPassword where

import           Control.Exception              ( evaluate )
import           Control.Monad                  ( when )
import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(..)
                                                , object
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Data.Word                      ( Word8 )
import           LandingPage.Handlers.Application.Tokens
                                                ( findUser
                                                , verifyAToken
                                                )
import           LandingPage.Handlers.Login.Utils
                                                ( hashPassword )
import           LandingPage.Utils              ( createTrace )
import           LandingPage.Validators         ( passwordCheck )
import           Network.HTTP.Types             ( status400
                                                , status403
                                                , status500
                                                )
import           Prelude                 hiding ( print
                                                , putStr
                                                , putStrLn
                                                ) -- Safety in not loggin credentials accidently
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , Password(..)
                                                , PhoneNumber(..)
                                                , UserID(..)
                                                , normalizeEmail
                                                )
import           Shared.TgthrMessages.Accounts  ( TokenMedium(..)
                                                , TokenVerificationFailure(..)
                                                , UserLoginFailureReason(..)
                                                )
import           Shared.Utils                   ( timeIOP )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , TraceContext
                                                , asClientM
                                                , incrementTrace
                                                )
import           System.Random                  ( randomIO )
import           Web.Scotty                     ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , param
                                                , status
                                                )

resetPassword
  :: ClientEnv
  -> TraceContext
  -> UserID
  -> Password
  -> IO (Either UserLoginFailureReason ())
resetPassword env trace uid (Password plainTextPassword) = do
  salt :: [Word8] <- mapM (const randomIO) [1 :: Integer .. 16]
  hashedPass      <- timeIOP "hashPassword"
    $ evaluate (hashPassword salt plainTextPassword)

  trace' <- incrementTrace trace
  let fn = _UserSetPassword asClientM trace' uid $ Password hashedPass

  res <- runClientM fn env
  case res of
    Right _ -> return $ Right ()
    Left  _ -> return $ Left UserDoesntExist

resetHandler :: ClientEnv -> ActionM ()
resetHandler env = do
  medium :: Text     <- param "medium"
  uniqueInfo :: Text <- param "identifier"
  token :: Text      <- param "token"
  userPassword       <- Password <$> param "password"

  let passwordFailure = isNothing $ passwordCheck userPassword

  (tokenMedium, email, phone) <- case medium of
    "email" -> return
      (EmailMedium, Just . normalizeEmail $ EmailAddress uniqueInfo, Nothing)
    "sms" -> return (PhoneMedium, Nothing, Just $ PhoneNumber uniqueInfo)
    _ ->
      status status400
        >> json
             (object
               [ "error" .= object
                 [ "passwordFailure" .= passwordFailure
                 , "mediumFailure" .= True
                 , "tokenFailure" .= Null
                 , ("message", "medium is bad")
                 ]
               , "token" .= token
               ]
             )
        >> finish

  when passwordFailure
    $  status status400
    >> json
         (object
           [ "error" .= object
             [ "passwordFailure" .= True
             , "mediumFailure" .= False
             , "tokenFailure" .= Null
             , ("message", "password is bad")
             ]
           , "token" .= token
           ]
         )
    >> finish

  trace     <- createTrace

  -- find user
  foundUser <- liftAndCatchIO $ findUser env trace email phone
  uid       <- case foundUser of
    Right u -> return u
    Left _ ->
      status status400
        >> json
             (object
               [ "error" .= object
                 [ "passwordFailure" .= False
                 , "mediumFailure" .= False
                 , "tokenFailure" .= Null
                 , ("message", "iden is bad")
                 ]
               , "token" .= token
               ]
             )
        >> finish

  -- verify Token
  isTokenOk <- liftAndCatchIO $ verifyAToken env trace uid tokenMedium token
  if isTokenOk
    then return ()
    else
      status status403
      >> json
           (object
             [ "error" .= object
               [ "passwordFailure" .= False
               , "mediumFailure" .= False
               , "tokenFailure" .= show TokenExpired
               , ("message", "token is bad")
               ]
             , "token" .= token
             ]
           )
      >> finish

  res <- liftAndCatchIO $ resetPassword env trace uid userPassword
  case res of
    Left e -> status status500 >> json
      (object
        [ "error" .= object
          [ "passwordFailure" .= False
          , "mediumFailure" .= False
          , "tokenFailure" .= Null
          , "message" .= show e
          ]
        , "token" .= token
        ]
      )
    Right _ -> json (object [])
