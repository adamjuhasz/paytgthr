{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Use second" -}
{- HLINT ignore "Use bimap" -}

{-# LANGUAGE OverloadedStrings #-}

module LandingPage.Handlers.Signup.GeneralUpdates where

import           Control.Monad                  ( unless )
import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(String)
                                                , object
                                                )
import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.Coerce                    ( coerce )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Templating.TemplateParams
                                                ( templateParamsForUserModel )
import           LandingPage.Types              ( EncryptedSSN(..)
                                                , SessionData
                                                )
import           LandingPage.Utils              ( AcceptType(..)
                                                , createTrace
                                                , expectSession
                                                , getAcceptType
                                                , requireUser
                                                )
import           LandingPage.Validators         ( ageVerification
                                                , emailCheck
                                                , isNotEmpty
                                                , nameValidator
                                                , phoneCheck
                                                , poboxCheck
                                                , ssnCheck
                                                , stack
                                                , stateCheck
                                                , zipCheck
                                                )
import           Network.HTTP.Types             ( status400
                                                , status409
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.User            as User
                                                ( UserID
                                                , UserModel(..)
                                                , UserState(..)
                                                , UserTrait(..)
                                                )
import           Shared.Vault                   ( PlainText(..) )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , TraceContext
                                                , UpdateUserBody(..)
                                                , asClientM
                                                , incrementTrace
                                                )
import qualified Web.Scotty                    as Scotty
import           Web.Scotty                     ( ActionM
                                                , finish
                                                , liftAndCatchIO
                                                , params
                                                , request
                                                , status
                                                )

encryptSelective
  :: ClientEnv
  -> TraceContext
  -> UserID
  -> (PlainText -> IO EncryptedSSN)
  -> (Text, Text)
  -> IO (Text, Maybe Text)
encryptSelective _ _ _ es ("ssn", "123120000") =
  es (PlainText "123120000") <&> coerce <&> Just <&> ("ssn", )
encryptSelective _ _ _ es ("ssn", "123120001") =
  es (PlainText "123120001") <&> coerce <&> Just <&> ("ssn", )
encryptSelective _ _ _ es ("ssn", "123120010") =
  es (PlainText "123120010") <&> coerce <&> Just <&> ("ssn", )
encryptSelective _ _ _ es ("ssn", "123120020") =
  es (PlainText "123120020") <&> coerce <&> Just <&> ("ssn", )
encryptSelective _ _ _ es ("ssn", "123120030") =
  es (PlainText "123120030") <&> coerce <&> Just <&> ("ssn", )
encryptSelective env trace uid es ("ssn", rawSSN) = do
  encrypted <- es (PlainText rawSSN) <&> coerce
  let fn = _UsersQuerySSN asClientM trace encrypted
  res <- runClientM fn env
  case res of
    Left e ->
      error $ "Error: encryptSelective _UsersQuerySSN " <> show (uid, e)
    Right [] -> return ("ssn", Just encrypted)
    Right users ->
      let filtered = filter (\u -> usrUserID u /= uid) users
      in  if null filtered
            then return ("ssn", Just encrypted)
            else return ("ssn", Nothing)
encryptSelective _ _ _ _ (key, val) = return (key, Just val)

validators :: UTCTime -> Text -> (Text -> Maybe Text)
validators _   "firstname" = nameValidator
validators _   "lastname"  = nameValidator
validators now "dob"       = ageVerification now
validators _   "phone"     = phoneCheck
validators _   "email"     = emailCheck
validators _   "ssn"       = ssnCheck
validators _   "state"     = stateCheck
validators _   "zip"       = zipCheck
validators _   "street"    = stack [poboxCheck, isNotEmpty]
validators _   "street2"   = poboxCheck
validators _   _           = isNotEmpty

keyTransformer :: Text -> UserTrait
keyTransformer "firstname"      = NameFirst
keyTransformer "lastname"       = NameLast
keyTransformer "street"         = AddressStreet
keyTransformer "street2"        = AddressStreet2
keyTransformer "city"           = AddressCity
keyTransformer "state"          = AddressState
keyTransformer "zip"            = AddressZip
keyTransformer "routing"        = FsACHBankRoutingNum
keyTransformer "account"        = FsACHBankAccountNum
keyTransformer "dob"            = DateOfBirth
keyTransformer "ssn"            = User.EncryptedSSN
keyTransformer "phone"          = Phone
keyTransformer "bankname"       = FsACHBankName
keyTransformer "accountname"    = FsACHBankAccountName
keyTransformer "banktype"       = FsACHBankAccountType
keyTransformer "dwollacustomer" = DwollaCustomerId
keyTransformer "dwollafunding"  = DwollaFundingId
keyTransformer t = error $ "Error: Don't understand key " <> show t

validate
  :: (Text -> (Text -> Maybe Text))
  -> [(Text, Text)]
  -> Either [Text] [(Text, Text)]
validate validateFuncs ins = if null errors then Right norms else Left errors
 where
  (errors, norms) = foldr doer ([], []) ins
  doer :: (Text, Text) -> ([Text], [(Text, Text)]) -> ([Text], [(Text, Text)])
  doer (key, val) (errs, normalized) = case validateFuncs key val of
    Nothing -> (errs <> [key], normalized)
    Just v  -> (errs, normalized <> [(key, v)])

nameRenderer :: [(Text, Value)] -> ActionM ()
nameRenderer inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object inParams
    TextHTML        -> Scotty.next

piiRenderer :: [(Text, Value)] -> ActionM ()
piiRenderer inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object inParams
    TextHTML        -> Scotty.next

addressRenderer :: [(Text, Value)] -> ActionM ()
addressRenderer inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object inParams
    TextHTML        -> Scotty.next

userUpdateHandler
  :: (PlainText -> IO EncryptedSSN)
  -> V.Key SessionData
  -> ([(Text, Value)] -> ActionM ())
  -> ClientEnv
  -> ActionM ()
userUpdateHandler encrypter sKey routeRenderer accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  info    <- params <&> lToSParams
  trace   <- createTrace
  accepts <- getAcceptType

  now     <- liftAndCatchIO getCurrentTime
  let getUser = _UserGet asClientM trace user
  thisUserM <- liftAndCatchIO $ runClientM getUser accountsEnv
  thisUser  <- case thisUserM of
    Left e -> do
      liftAndCatchIO $ putStr "Error: userUpdateHandler _UserGet" >> print
        (user, e)
      status status500 >> Scotty.json (object []) >> finish
    Right u -> return u

  let trimmedInfo     = fmap (\(k, v) -> (k, T.strip v)) info
      eitherValidated = validate (validators now) trimmedInfo

  -- Only allow updates if user is the following states
  -- UserCreated, UserWaitingOnPII, UserWaitingOnKYC
  getTrace   <- incrementTrace trace
  userModelE <- liftAndCatchIO
    $ runClientM (_UserGet asClientM getTrace user) accountsEnv
  userModel <- case userModelE of
    Left e -> do
      liftAndCatchIO
        $  putStr "Error: userUpdateHandler can't get user "
        >> print (user, e)
      status status500 >> Scotty.json (object []) >> finish
    Right model -> return model

  let errorBadUserState = status status409 >> Scotty.json (object []) >> finish

  _ <- case userModel of
    UserModel { usrUserState = UserCreated }         -> return ()
    UserModel { usrUserState = UserWaitingOnPII }    -> return ()
    UserModel { usrUserState = UserWaitingOnKYC }    -> return ()
    UserModel { usrUserState = UserKYCDelay }        -> errorBadUserState
    UserModel { usrUserState = UserUpdated }         -> errorBadUserState
    UserModel { usrUserState = UserUpdatedKYCDelay } -> errorBadUserState
    UserModel { usrUserState = UserActive }          -> errorBadUserState
    UserModel { usrUserState = UserClosed _ }        -> errorBadUserState

  validated <- case eitherValidated of
    Left e -> do
      let templateParams =
            templateParamsForUserModel thisUser
              <> fmap (\err -> (err <> "-error") .= True)    e
              <> fmap (\(k, v) -> (k <> "-value", String v)) info
      status status400 >> routeRenderer templateParams >> finish
    Right v -> return v

  encryptedVals <- liftAndCatchIO
    $ mapM (encryptSelective accountsEnv trace user encrypter) validated
  let allEncrypted =
        foldr (\(_, v) accum -> accum && isJust v) True encryptedVals
  unless
    allEncrypted
    (do
      let templateParams =
            templateParamsForUserModel thisUser
              <> ["ssn-reuse-error" .= True]
              <> fmap (\(k, v) -> (k <> "-value", String v)) info
      status status400 >> routeRenderer templateParams >> finish
    )

  let finalVals = first keyTransformer <$> encryptedVals

  result <- liftAndCatchIO $ do
    trace' <- incrementTrace trace
    let fn = _UserUpdate asClientM trace' user $ UpdateUserBody finalVals
    res <- runClientM fn accountsEnv
    case res of
      Left e -> do
        putStr "Error: userUpdateHandler _UserUpdate " >> print (user, e)
        return $ Left ("Upstream failure" :: String)
      Right _ -> return $ Right ()

  case (result, accepts) of
    (Left err, _) ->
      error $ "Error: userUpdateHandler UpdateUser " <> show (user, err)
    (Right _, TextHTML       ) -> Scotty.next
    (Right _, ApplicationJSON) -> Scotty.json $ object []
  where lToSParams = fmap (\(k, v) -> (TL.toStrict k, TL.toStrict v))
