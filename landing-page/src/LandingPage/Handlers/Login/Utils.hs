module LandingPage.Handlers.Login.Utils where

import qualified Crypto.Argon2                 as Ar
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Short               as TS
import           Data.Word                      ( Word8 )
import           LandingPage.Utils              ( AcceptType
                                                  ( ApplicationJSON
                                                  , TextHTML
                                                  )
                                                , getAcceptType
                                                )
import           Network.HTTP.Types             ( status400 )
import           Prelude                 hiding ( print
                                                , putStr
                                                , putStrLn
                                                ) -- Safety in not loggin credentials accidently
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , status
                                                , text
                                                )

hashPassword :: [Word8] -> T.Text -> T.Text
hashPassword salt p = TS.toText $ case hashAttempt of
  Left  err    -> Prelude.error $ show err
  Right hashed -> hashed
 where
  hashOptions = Ar.defaultHashOptions { Ar.hashParallelism = 1
                                      , Ar.hashMemory      = 2 ^ (14 :: Int)
                                      , Ar.hashIterations  = 4
                                      }
  hashAttempt = Ar.hashEncoded hashOptions (TE.encodeUtf8 p) (B.pack salt)

loginRescue :: TL.Text -> ActionM a
loginRescue e = do
  accepts <- getAcceptType
  status status400
  case accepts of
    TextHTML        -> text ("Malformed: " <> e)
    ApplicationJSON -> Scotty.json $ object ["error" .= e]
  finish
