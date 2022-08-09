module LandingPage.TokenUtils where

import           Control.Exception
import           Data.Coerce
import           Data.Functor
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock
import           Data.Time.Format
import           LandingPage.Types              ( EncryptedToken(..) )
import           Shared.Utils
import           Shared.Vault

createToken :: (PlainText -> IO EncryptedToken) -> Text -> IO EncryptedToken
createToken encrypter dataToEnc = do
  now <- getCurrentTime <&> formatTime defaultTimeLocale "%Y-%m-%d" <&> T.pack
  let plainText = PlainText $ T.concat [dataToEnc, ":", now]
  cipher <- encrypter plainText <&> coerce
  return . EncryptedToken . b64toB64URL $ T.drop 6 cipher

data ReadTokenErrors
  = MissingKeyVersion
  | BadTokenFormat
  | BadTimeFormat
  | TokenExpired Text
  deriving (Eq, Show)
instance Exception ReadTokenErrors

readToken
  :: (EncryptedToken -> IO PlainText)
  -> EncryptedToken
  -> IO (Either ReadTokenErrors Text)
readToken decrypter token = try $ tokenDecrypter decrypter token

tokenDecrypter :: (EncryptedToken -> IO PlainText) -> EncryptedToken -> IO Text
tokenDecrypter decrypter (EncryptedToken token) = do
  (tokenVer, tokenContent) <- case T.splitOn ":" token of
    [x, y] -> return (x, y)
    _      -> throw MissingKeyVersion

  let convToken = EncryptedToken
        $ T.concat ["vault:", tokenVer, ":", b64URLToB64 tokenContent]

  decTokenE :: Either SomeException PlainText <- try $ decrypter convToken
  let (PlainText decToken) = case decTokenE of
        Left  _   -> throw BadTokenFormat
        Right tok -> tok
  now <- getCurrentTime

  let (contents, time) = case T.splitOn ":" decToken of
        [x, y] -> (x, y)
        _      -> throw BadTokenFormat

  let maxTokenTime = nominalDay * 7
      isExpired =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack time)
          <&> diffUTCTime now
          <&> (<) maxTokenTime

  case isExpired of
    Nothing    -> throw BadTimeFormat
    Just True  -> throw $ TokenExpired contents
    Just False -> return contents
