{-# LANGUAGE QuasiQuotes #-}

module APIPrivacy.ExternalAPI.PinEncryption where

import           Crypto.Hash.Algorithms         ( SHA1(..) )
import qualified Crypto.PubKey.RSA.OAEP        as RSA
import           Crypto.PubKey.RSA.Types        ( PublicKey(..) )
import           Crypto.Random                  ( MonadRandom )
import           Crypto.Store.X509              ( readPubKeyFileFromMemory )
import           Data.ByteArray.Encoding        ( Base(Base64)
                                                , convertToBase
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C8
import qualified Data.X509                     as X509
import           Text.RawString.QQ              ( r )

convertPub :: X509.PubKey -> PublicKey
convertPub (X509.PubKeyRSA p) = p
convertPub _                  = error "Unsported PubKey"

pubKey :: PublicKey
pubKey =
  convertPub
    . head
    . readPubKeyFileFromMemory
    . C8.pack
    $ [r|-----BEGIN RSA PUBLIC KEY-----
MIICCgKCAgEAuBxXhZjTv6OGsFNJC5SoBYT3KArN4m98ywzXYe5lihY0F08K8Mla
+YzseES+f3ZwPUK2RJnhQlotOE4qAtB7/BCNsZB2FPnFUl8hQceR16pnhkeEOJYL
vgtN97Kfldil4ge8/k2qhg3j1brhXX9qUM+1jXGFmFdNM/A+4ox5OJrjxEUbNffz
diztw73qWZeb8rA0sb8MDhAfNXty0nE6ggx33uHScyQO2NjYYBs2OXMsdN5LvP9/
7ggsmq7vr2V72G72COD4jQ4cBxEnhboJm13i/Ow2ophX6wMlGXAJ4PiuZ/vw2Qm5
L9ptr/VIpqfz2jeoUa3ljQZ2NO+MxQkSf9h62xu+O+Gd1D83n7GxwfUYXNwQYv/+
y7HyYWi65JEOCIfjE+mppWh8UsL+6LSqHXC8Mraq9cpI6vNKQcKWCdvkgPIOUTPj
eCQeLjJ5aHB7h2rvdWC2a3qyJjqigUKYF+oRVOzav6iS5o/Id3XQ6rOpDq5LK4fY
vc6bBdXM6ky3enyEhAEzjDbxY9bbRvApTGbVZzfvyKbG/UP0KHFJca8djJvbNvv9
yd/eGsDlnCd4ROwG8a0Moo6ALM+oer3XvLYV1JLnSd7GO7AgI7An7lFdXFYy1WlT
OlHHRT3KG977GhHjCsV+g1vH5JmcJGLPBi+xbvVhUIj5YRPZfsn4uJkCAwEAAQ==
-----END RSA PUBLIC KEY-----|]

encryptPin :: (MonadRandom m) => ByteString -> m ByteString
encryptPin input = do
  let params = RSA.defaultOAEPParams SHA1
  res <- RSA.encrypt params pubKey input
  case res of
    Left  er -> error $ "Error: Can't OAEP encrypt " <> show er
    Right bs -> return $ convertToBase Base64 bs
