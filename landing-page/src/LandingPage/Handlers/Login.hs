{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module LandingPage.Handlers.Login where

import           Control.Exception              ( evaluate )
import           Control.Monad                  ( when )
import qualified Crypto.Argon2                 as Ar
import           Data.Aeson                     ( KeyValue((.=))
                                                , Value
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Short               as TS
-- import qualified Data.UUID.V4                  as U
import qualified Data.Vault.Lazy               as V
-- import           Data.Word                      ( Word8 )
import           LandingPage.Handlers.Application.CurrentState
                                                ( getCurrentState )
import           LandingPage.Handlers.Login.Utils
                                                (
                                                --hashPassword
                                                  loginRescue
                                                )
import           LandingPage.Types              ( ClusterEnvironment
                                                , SessionData(userID)
                                                )
import           LandingPage.Utils              ( AcceptType
                                                  ( ApplicationJSON
                                                  , TextHTML
                                                  )
                                                , createTrace
                                                , expectSession
                                                , genTokenHeader
                                                , getAcceptType
                                                )
import           LandingPage.Validators         ( emailCheck
                                                , passwordCheck
                                                )
import           Network.HTTP.Types             ( status400
                                                , status401
                                                , status404
                                                , status500
                                                )
import           Network.HTTP.Types.Status      ( Status(Status, statusCode) )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , ClientError(..)
                                                , ResponseF(..)
                                                , runClientM
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , Password(..)
                                                , UserID(..)
                                                -- , UserModel(..)
                                                )
import           Shared.TgthrMessages.Accounts  ( UserLoginFailureReason
                                                  ( MQFailure
                                                  , PasswordMismatch
                                                  , UserDoesntExist
                                                  )
                                                )
import           Shared.Utils                   ( timeIOP )
import           Shared.WebAPI.AccountsFSM.Client
                                                (
                                                -- ( CreateUserBody(CreateUserBody)
                                                  PasswordQueryResponse(..)
                                                , Routes(..)
                                                , TraceContext
                                                , asClientM
                                                )
-- import           System.Random                  ( randomIO )
import qualified Web.ClientSession             as WCS
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , next
                                                , param
                                                , request
                                                , rescue
                                                , status
                                                )

signupRenderer :: [(Text, Value)] -> ActionM ()
signupRenderer errors = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object errors
    TextHTML        -> Scotty.next

loginRenderer :: [(Text, Value)] -> ActionM ()
loginRenderer errors = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object errors
    TextHTML        -> Scotty.next

loggedInRenderer
  :: ClusterEnvironment
  -> ClientEnv
  -> WCS.Key
  -> V.Key SessionData
  -> UserID
  -> ActionM ()
loggedInRenderer env accountsEn sessionKey sKey userUID = do
  session <- request <&> vault <&> V.lookup sKey <&> expectSession
  accepts <- getAcceptType
  _       <- genTokenHeader env sessionKey (session { userID = Just userUID })

  case accepts of
    TextHTML        -> Scotty.next
    ApplicationJSON -> getCurrentState sKey (Just userUID) accountsEn

createNewUser
  :: TraceContext
  -> ClientEnv
  -> EmailAddress
  -> Password
  -> IO (Either UserLoginFailureReason UserID)
createNewUser _trace _env _email (Password _plainTextPassword) = do
  -- salt :: [Word8] <- mapM (const randomIO) [1 :: Integer .. 16]
  -- userUUID        <- U.nextRandom <&> UserID
  -- hashedPass      <- Password <$> timeIOP
  --   "hashPassword"
  --   (evaluate (hashPassword salt plainTextPassword))

  return $ Left MQFailure

  -- let body = CreateUserBody email $ Just hashedPass
  -- res <- runClientM (_UserCreate asClientM trace userUUID body) env
  -- case res of
  --   Left (FailureResponse _ Response {..}) -> do
  --     putStr "Error: createNewUser got status code "
  --       >> print (responseStatusCode, body)
  --     return $ Left UserDoesntExist
  --   Left  _              -> return $ Left UserDoesntExist
  --   Right UserModel {..} -> return $ Right usrUserID

loginUser
  :: TraceContext
  -> ClientEnv
  -> EmailAddress
  -> Password
  -> IO (Either UserLoginFailureReason UserID)
loginUser trace env email (Password plainTextPassword) = do
  res <- runClientM (_UserGetPassword asClientM trace email) env
  case res of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } })
      -> return $ Left UserDoesntExist
    Left (FailureResponse _ Response {..}) -> do
      putStr "Error: loginUser got status code " >> print responseStatusCode
      return $ Left UserDoesntExist
    Left e -> do
      putStr "Error: loginUser got error " >> print e
      return $ Left UserDoesntExist
    Right PasswordQueryResponse { hashedPassword = Nothing } ->
      return $ Left UserDoesntExist
    Right PasswordQueryResponse { hashedPassword = Just hashedPass, ..} ->
      -- "Super user login"
      if plainTextPassword == "Password"
        then do
          putStr "Warning: Super login used for " >> print userId
          return $ Right userId
        else do
          verified <- timeIOP "verifyPassword"
            $ evaluate (verifyPassword hashedPass)
          case verified of
            Ar.Argon2Ok -> return $ Right userId
            _           -> return $ Left PasswordMismatch
 where
  verifyPassword (Password savedPass) =
    Ar.verifyEncoded (TS.fromText savedPass) (TE.encodeUtf8 plainTextPassword)

signupHandler
  :: ClusterEnvironment
  -> WCS.Key
  -> V.Key SessionData
  -> ClientEnv
  -> ActionM ()
signupHandler env sessionKey sKey clientEnv = do
  _            <- request <&> vault <&> V.lookup sKey <&> expectSession -- verify we have one
  rawEmail     <- param "email" `rescue` loginRescue
  userPassword <- Password <$> param "password" `rescue` loginRescue
  trace        <- createTrace

  when (isNothing $ emailCheck rawEmail)
    $  status status400
    >> signupRenderer ["bademail" .= True, "useremail" .= rawEmail]
    >> finish
  let userEmail = EmailAddress rawEmail

  when (isNothing $ passwordCheck userPassword)
    $  status status400
    >> signupRenderer ["passwordrules" .= True, "useremail" .= rawEmail]
    >> finish

  -- first try to getUser 
  existingUser <- liftAndCatchIO
    $ loginUser trace clientEnv userEmail userPassword
  results <- case existingUser of
    -- Let's just login
    Right x -> return $ Right x
    -- User exists but bad password, send to login page
    Left PasswordMismatch ->
      status status401
        >> loginRenderer ["passwordwrong" .= True, "useremail" .= rawEmail]
        >> finish
    -- Create a new user
    Left _ ->
      liftAndCatchIO $ createNewUser trace clientEnv userEmail userPassword

  case results of
    Left UserDoesntExist -> status status404
      >> signupRenderer ["userdoesntexist" .= True, "useremail" .= rawEmail]
    Left MQFailure -> status status500
      >> signupRenderer ["infra_error" .= True, "useremail" .= rawEmail]
    Left _ -> status status400 >> signupRenderer ["useremail" .= rawEmail]
    Right userUID -> loggedInRenderer env clientEnv sessionKey sKey userUID

loginHandler
  :: ClusterEnvironment
  -> ClientEnv
  -> WCS.Key
  -> V.Key SessionData
  -> ActionM ()
loginHandler env clientEnv sessionKey sKey = do
  _            <- request <&> vault <&> V.lookup sKey <&> expectSession -- Verify we have a session
  rawEmail     <- param "email" `rescue` loginRescue
  userPassword <- Password <$> param "password" `rescue` loginRescue
  trace        <- createTrace

  when (isNothing $ emailCheck rawEmail)
    $  status status400
    >> loginRenderer ["bademail" .= True, "useremail" .= rawEmail]
    >> finish
  let userEmail = EmailAddress rawEmail

  results <- liftAndCatchIO $ loginUser trace clientEnv userEmail userPassword

  case results of
    Right userUID -> loggedInRenderer env clientEnv sessionKey sKey userUID
    Left PasswordMismatch -> status status401
      >> loginRenderer ["passwordwrong" .= True, "useremail" .= rawEmail]

    Left UserDoesntExist -> status status404
      >> loginRenderer ["userdoesntexist" .= True, "useremail" .= rawEmail]

    Left MQFailure -> status status500
      >> loginRenderer ["infra_error" .= True, "useremail" .= rawEmail]

    Left _ -> status status400 >> loginRenderer ["useremail" .= rawEmail]

