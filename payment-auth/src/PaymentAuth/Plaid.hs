{-# LANGUAGE NamedFieldPuns, RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}

module PaymentAuth.Plaid where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( (.:)
                                                , Object
                                                , Value(String)
                                                , eitherDecode
                                                , encode
                                                , withObject
                                                )
import           Data.Aeson.Types               ( parseEither )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Foldable                  ( asum )
import           Data.Function                  ( (&) )
import qualified Data.HashMap.Strict           as Hm
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool                      ( Pool
                                                , withResource
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Network.HTTP.Client            ( Manager
                                                , ManagerSettings(..)
                                                , Request(..)
                                                , RequestBody(RequestBodyLBS)
                                                , Response(..)
                                                , httpLbs
                                                , newManager
                                                , parseRequest
                                                , responseTimeoutMicro
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Types.Status      ( statusCode )
import           Shared.Models.Plaid.Base       ( ACH(..)
                                                , AccessToken(..)
                                                , Account(..)
                                                , AuthResponse(..)
                                                , BalanceResponse(..)
                                                , ItemId(..)
                                                , Numbers(..)
                                                , PlaidAccountId
                                                )
import           Shared.TgthrMessages.Base      ( AccountType(Depository)
                                                , DepositoryType(..)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AccountDetails(..)
                                                , PlaidEnvironment(..)
                                                , PublicToken(..)
                                                )
import           Shared.Utils                   ( eStoT
                                                , timeIO
                                                )

type PlaidSecret = Text
type URLFragment = String

type Requester
  =  PlaidEnvironment
  -> URLFragment
  -> Object
  -> IO (Either Text (Response ByteString))

data PlaidSecrets = PlaidSecrets
  { clientId :: Text
  , secret   :: PlaidSecret
  , url      :: String
  }
  deriving Eq

createMananger :: b -> IO (Manager, b)
createMananger secrets = do
  Prelude.putStrLn "Creating a new manager"
  manager <- newManager $ tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro 60000000
    }
  return (manager, secrets)

secretForEnv :: PlaidSecrets -> PlaidEnvironment -> PlaidSecret
secretForEnv PlaidSecrets {..} env = case env of
  Sandbox     -> secret
  Development -> secret
  Production  -> secret

urlForEnv :: PlaidSecrets -> PlaidEnvironment -> URLFragment
urlForEnv PlaidSecrets {..} env = case env of
  Sandbox     -> url
  Development -> url
  Production  -> url

requestWithPool :: Pool (Manager, PlaidSecrets) -> Requester
requestWithPool pool plaidEnv aPath reObject =
  withResource pool $ \(manager, secrets) ->
    generateRequest secrets manager plaidEnv aPath reObject

generateRequest :: PlaidSecrets -> Manager -> Requester
generateRequest secrets manager plaidEnv aPath requestObject = do
  let secretObject = Hm.fromList
        [ ("secret"   , String $ secretForEnv secrets plaidEnv)
        , ("client_id", String $ clientId secrets)
        ]
      filledObject   = Hm.union secretObject requestObject
      theRequestBody = encode filledObject
  request' <- parseRequest $ urlForEnv secrets plaidEnv <> aPath
  let request = request'
        { method         = "POST"
        , requestBody    = RequestBodyLBS theRequestBody
        , requestHeaders = [ ("Content-Type", "application/json")
                           , ("User-Agent", "Pay Tgthr 1.0, adam@example.com")
                           ]
        }
      excRequest = httpLbs request manager
      controlledRequest =
        try excRequest :: IO (Either SomeException (Response ByteString))
  (_, result) <- timeIO (Just aPath) controlledRequest
  case result of
    Left  exc      -> return $ Left $ T.pack $ show exc
    Right response -> if statusCode (responseStatus response) /= 200
      then return $ Left $ T.pack $ show $ responseBody response
      else return $ Right response

exchangeToken
  :: (MonadIO m)
  => Requester
  -> PublicToken
  -> PlaidEnvironment
  -> m (Either Text (AccessToken, ItemId))
exchangeToken request (PublicToken thePublicToken) plaidEnv = do
  let fn = request plaidEnv "/item/public_token/exchange" requestObject
  res <- liftIO fn
  let decoded = res >>= decoder >>= parser
  return decoded
 where
  requestObject     = Hm.fromList [("public_token", String thePublicToken)]
  accessTokenParser = withObject
    "access_token"
    (\o ->
      (,)
        <$> (AccessToken <$> o .: "access_token")
        <*> (ItemId <$> o .: "item_id")
    )
  decoder :: Response ByteString -> Either Text Value
  decoder = eStoT . eitherDecode . responseBody
  parser  = eStoT . parseEither accessTokenParser

getAuth
  :: (MonadIO m)
  => Requester
  -> (AccessToken, PlaidEnvironment)
  -> m (Either Text AuthResponse)
getAuth request (AccessToken atoken, plaidEnv) = do
  let fn = request plaidEnv "/auth/get" requestObject
  res <- liftIO fn
  let decoded = res >>= (eStoT . eitherDecode . responseBody)
  return decoded
  where requestObject = Hm.fromList [("access_token", String atoken)]

getBalance
  :: (MonadIO m)
  => Requester
  -> (AccessToken, PlaidEnvironment)
  -> m (Either Text (Double, BalanceResponse))
getBalance request (AccessToken atoken, plaidEnv) = do
  res <- liftIO $ request plaidEnv "/accounts/balance/get" requestObject
  let decoded = res >>= (eStoT . eitherDecode . responseBody)
  return decoded
  where requestObject = Hm.fromList [("access_token", String atoken)]

getAccountList
  :: (MonadIO m)
  => Requester
  -> (AccessToken, PlaidEnvironment)
  -> m (Either Text [Account])
getAccountList request tokenPlus = do
  res <- getBalance request tokenPlus
  let list = processBalances . snd <$> res
  return list

processBalances :: BalanceResponse -> [Account]
processBalances balanceResponse =
  balAccounts balanceResponse & filter isCheckingSavings

isCheckingSavings :: Account -> Bool
isCheckingSavings acct = case accountType acct of
  Depository Savings  -> True
  Depository Checking -> True
  _                   -> False

foldToDetails
  :: (PlaidAccountId -> Maybe ACH)
  -> Account
  -> [AccountDetails]
  -> [AccountDetails]
foldToDetails findACH Account {..} accum = case findACH accountId of
  Nothing -> accum
  Just ach ->
    AccountDetails
        { acctId         = accountId
        , acctName       = fromMaybe (defName accountType)
                                     (asum [accountName, accountOfficialName])
        , acctABARouting = achABARouting ach
        , acctDDNNumber  = achDDANumber ach
        , acctCurrBal    = accountCurrentBalance
        , acctType       = accountType
        }
      : accum
  where defName aType = T.pack $ show aType

extractChecking :: AuthResponse -> [AccountDetails]
extractChecking AuthResponse { authAccounts, authNumbers = Numbers { ach } } =
  foldr (foldToDetails (findACH ach)) [] usableAccts
 where
  usableAccts = filter isCheckingSavings authAccounts
  findACH :: [ACH] -> PlaidAccountId -> Maybe ACH
  findACH [] _ = Nothing
  findACH (anAch@ACH {..} : achs) aid | aid == achAccountId = Just anAch
                                      | otherwise           = findACH achs aid
