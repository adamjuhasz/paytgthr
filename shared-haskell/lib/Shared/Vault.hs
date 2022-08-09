{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE StrictData, RecordWildCards #-}

module Shared.Vault where

import           Control.Retry                  ( constantDelay
                                                , limitRetries
                                                , recoverAll
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                , FromJSON(..)
                                                , Result(..)
                                                , Value(..)
                                                , eitherDecode
                                                , encode
                                                , fromJSON
                                                , object
                                                , withObject
                                                )
import           Data.Aeson.Types               ( Object )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as BC8
import           Data.Coerce                    ( coerce )
import           Data.Functor                   ( (<&>) )
import qualified Data.HashMap.Strict           as HMS
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           GHC.Stack                      ( HasCallStack )
import           Network.Connection             ( TLSSettings
                                                  ( TLSSettingsSimple
                                                  , settingDisableCertificateValidation
                                                  , settingDisableSession
                                                  , settingUseServerName
                                                  )
                                                )
import           Network.HTTP.Client            ( Manager
                                                , Request
                                                  ( requestBody
                                                  , requestHeaders
                                                  )
                                                , RequestBody(RequestBodyLBS)
                                                , Response
                                                  ( responseBody
                                                  , responseHeaders
                                                  , responseStatus
                                                  )
                                                , httpLbs
                                                , parseRequest
                                                )
import           Network.HTTP.Client.TLS        ( mkManagerSettings
                                                , newTlsManagerWith
                                                )
import           Network.HTTP.Types             ( Status(statusCode) )
import           System.Exit                    ( ExitCode(..) )
import           System.Posix.Process           ( exitImmediately )

type VaultToken = ByteString

readTokenFile :: String -> IO ByteString
readTokenFile tokenFile = recoverAll
  (constantDelay 1000000 <> limitRetries 7) -- ~127 seconds (2 min)
  (const (putStrLn "Trying to open tokenfile" >> BC8.readFile tokenFile))

data TransitAction
  = Encrypt
  | Decrypt
  deriving (Eq, Show)

toStr :: TransitAction -> String
toStr Encrypt = "encrypt"
toStr Decrypt = "decrypt"

newtype KeyName = KeyName String deriving (Eq, Show)
type EncryptService = TransitAction -> Value -> IO (Either String Value)

data VaultResponse = VaultResponse
  { dataPayload :: Value
  , requestId   :: Text
  }
  deriving (Eq, Show)
instance FromJSON VaultResponse where
  parseJSON = withObject "VaultResponse" $ \o -> do
    dataPayload <- o .: "data"
    requestId   <- o .: "request_id"
    return $ VaultResponse { .. }

newtype Decryption = Decryption
  { plaintext ::  PlainText
  } deriving (Eq, Show)
instance FromJSON Decryption where
  parseJSON = withObject "Decryption" $ \o -> do
    p <- o .: "plaintext" <&> encodeUtf8 <&> B64.decode
    case p of
      Left s -> fail s
      Right plaintext ->
        return $ Decryption { plaintext = PlainText $ decodeUtf8 plaintext }

data VaultSecretResponse = VaultSecretResponse
  { secData    :: Object
  , secVersion :: Int
  }
  deriving (Eq, Show)
instance FromJSON VaultSecretResponse where
  parseJSON = withObject "VaultSecretResponse" $ \o -> do
    secData    <- o .: "data"

    metadata   <- o .: "metadata"
    secVersion <- metadata .: "version"

    return $ VaultSecretResponse { .. }

data VaultCredResponse = VaultCredResponse
  { vcUsername :: Text
  , vcPassword :: Text
  }
  deriving (Eq, Show)
instance FromJSON VaultCredResponse where
  parseJSON = withObject "VaultCredResponse" $ \o -> do
    vcUsername <- o .: "username"
    vcPassword <- o .: "password"
    return $ VaultCredResponse { .. }

newtype Encryption = Encryption
  { ciphertext :: CipherText
  } deriving (Eq, Show)
instance FromJSON Encryption where
  parseJSON = withObject "Decryption" $ \o -> do
    ciphertext <- CipherText <$> o .: "ciphertext"
    return $ Encryption { .. }

base64Encoder :: Text -> Text
base64Encoder = decodeUtf8 . B64.encode . encodeUtf8

newtype Context = Context Text deriving (Eq, Show)
instance IsString Context where
  fromString = Context . T.pack
newtype PlainText = PlainText Text deriving (Eq, Show)
newtype CipherText = CipherText Text deriving (Eq, Show)

encrypt
  :: HasCallStack
  => EncryptService
  -> Maybe Context
  -> PlainText
  -> IO CipherText
encrypt run context (PlainText input) = do
  res <- run Encrypt $ object
    [ "plaintext" .= base64Encoder input
    , "context" .= (context <&> coerce <&> base64Encoder)
    ]
  case res >>= eitherResult . fromJSON of
    Left s -> do
      putStr "Error: Encryption failed due to, restarting" >> print s
      exitImmediately $ ExitFailure 42
      error $ "Encryption failed due to: " <> s
    Right t -> return $ ciphertext t

decrypt
  :: HasCallStack
  => EncryptService
  -> Maybe Context
  -> CipherText
  -> IO PlainText
decrypt run context (CipherText input) = do
  res <- run Decrypt $ object
    ["ciphertext" .= input, "context" .= (context <&> coerce <&> base64Encoder)]
  case res >>= eitherResult . fromJSON of
    Left s -> do
      putStr "Error: Decryption failed due to, restarting " >> print s
      exitImmediately $ ExitFailure 42
      error "Error: Decryption failed"
    Right r -> return $ plaintext r

getSecret
  :: Manager -> String -> VaultToken -> String -> IO (Either String Object)
getSecret man base token key = do
  let url = "GET " <> base <> "/v1/kv/data/" <> key
  req <- parseRequest url
    <&> \r -> r { requestHeaders = [("X-Vault-Token", token)] }
  res <- httpLbs req man

  let currPayload = case resCode res of
        200 ->
          (eitherDecode . responseBody $ res)
            >>= (eitherResult . fromJSON . dataPayload)
        _ -> Left $ genError res
      currVersion = currPayload <&> secData

  return currVersion
 where
  resCode = statusCode . responseStatus
  genError res = "Got HTTP status code (" <> show (resCode res) <> ")"

getSecretOrThrow :: Manager -> String -> VaultToken -> String -> IO Object
getSecretOrThrow man base token key = getSecret man base token key <&> \x ->
  case x of
    Left  s -> error $ "Error: secret failed to get: " <> key <> " b/c: " <> s
    Right y -> y

extractSecret :: Object -> Text -> String
extractSecret secrets key = case secrets HMS.! key of
  String t -> T.unpack t
  _        -> error $ "Error: no value for " <> show key

getRMQCreds
  :: Manager
  -> String
  -> VaultToken
  -> String
  -> IO (Either String VaultCredResponse)
getRMQCreds man base token rmqRole = do
  let url = "GET " <> base <> "/v1/rabbitmq/creds/" <> rmqRole
  req <- parseRequest url
    <&> \r -> r { requestHeaders = [("X-Vault-Token", token)] }
  res <- httpLbs req man

  return $ case resCode res of
    200 ->
      (eitherDecode . responseBody $ res)
        >>= (eitherResult . fromJSON . dataPayload)
    _ -> Left $ genError res
 where
  resCode = statusCode . responseStatus
  genError res = "Got HTTP status code (" <> show (resCode res) <> ")"

data VaultCertResponse = VaultCertResponse
  { certificate      :: Text
  , issuing_ca       :: Text
  , ca_chain         :: Maybe [Text]
  , private_key      :: Text
  , private_key_type :: Text
  , serial_number    :: Text
  }
  deriving (Eq, Show)
instance FromJSON VaultCertResponse where
  parseJSON = withObject "VaultCertResponse" $ \o -> do
    certificate      <- o .: "certificate"
    issuing_ca       <- o .: "issuing_ca"
    ca_chain         <- o .:? "ca_chain"
    private_key      <- o .: "private_key"
    private_key_type <- o .: "private_key_type"
    serial_number    <- o .: "serial_number"
    return $ VaultCertResponse { .. }

type CommmonName = Text
getPKICert
  :: Manager
  -> String
  -> VaultToken
  -> CommmonName
  -> IO (Either String VaultCertResponse)
getPKICert man base token commonName = do
  let url = "POST " <> base <> "/v1/pki/issue/service-tls-role"
  req <- parseRequest url <&> \r -> r
    { requestHeaders = [ ("X-Vault-Token", token)
                       , ("Content-Type" , "application/json")
                       , ("Accept"       , "application/json")
                       ]
    , requestBody    = RequestBodyLBS . encode $ object
                         ["common_name" .= commonName]
    }
  res <- httpLbs req man

  return $ case resCode res of
    200 ->
      (eitherDecode . responseBody $ res)
        >>= (eitherResult . fromJSON . dataPayload)
    _ -> Left $ genError res
 where
  resCode = statusCode . responseStatus
  genError res = "Got HTTP status code (" <> show (resCode res) <> ")"
eitherResult :: Result a -> Either String a
eitherResult v = case v of
  Error   s -> Left s
  Success a -> Right a

postEncryption :: Manager -> String -> VaultToken -> KeyName -> EncryptService
postEncryption man base token (KeyName key) action payload = do
  let url = "POST " <> base <> "/v1/transit/" <> toStr action <> "/" <> key
  req <- parseRequest url <&> \r -> r
    { requestHeaders = [("X-Vault-Token", token)]
    , requestBody    = RequestBodyLBS $ encode payload
    }
  res <- httpLbs req man
  case resCode res of
    200 -> return ((eitherDecode . responseBody $ res) <&> dataPayload)
    403 -> do
      putStr "Error: postEncryption now forbidden, restarting "
        >> print (resCode res, responseBody res, responseHeaders res)
      exitImmediately $ ExitFailure 42
      return . Left $ genError res
    _ -> do
      putStr "Error: postEncryption "
        >> print (resCode res, responseBody res, responseHeaders res)
      return . Left $ genError res
 where
  resCode = statusCode . responseStatus
  genError res =
    "postEncryption got HTTP status response ("
      <> show (resCode res)
      <> ", "
      <> show (responseBody res)
      <> ")"

initManager :: IO Manager
initManager = newTlsManagerWith (mkManagerSettings tlsSettings Nothing)
 where
  tlsSettings = TLSSettingsSimple { settingDisableCertificateValidation = True
                                  , settingDisableSession               = False
                                  , settingUseServerName                = False
                                  }

