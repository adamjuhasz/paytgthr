{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- Needed for stack 17.10 for import Control.Monad.Fail

module Shared.Utils where

import           Control.Monad.Fail             ( MonadFail(..) )
import qualified Data.Aeson                    as A
import qualified Data.Char                     as C
import           Data.List                      ( isPrefixOf )
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import qualified Data.Time                     as DT
import           GHC.Stack                      ( HasCallStack )
import           Prelude
import qualified Prelude                       as P

flowM :: Monad m => (a -> m b) -> a -> m a
flowM f v = do
  _ <- f v
  return v

(>>\=) :: Monad m => m a -> (a -> m b) -> m a
(>>\=) a b = a >>= flowM b

eStoT :: Either String a -> Either T.Text a
eStoT x = case x of
  Left  e -> Left $ T.pack e
  Right v -> Right v

timeIO :: Maybe String -> IO a -> IO (Double, a)
timeIO sm action = do
  start <- DT.getCurrentTime
  x     <- action
  end   <- DT.getCurrentTime
  case sm of
    Nothing -> return ()
    Just s  -> putStr (s <> " took: ") >> print (DT.diffUTCTime end start)
  return (realToFrac $ DT.diffUTCTime end start, x)

timeIOP :: String -> IO a -> IO a
timeIOP s action = do
  (_, x) <- timeIO (Just s) action
  return x

fromRight :: (HasCallStack, Show a) => Either a b -> b
fromRight (Left  err) = error $ "fromRight: (Left " <> show err <> ")"
fromRight (Right b  ) = b

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right x) = Just x

instance MonadFail (Either String) where
  fail = Left

stringToTime :: HasCallStack => String -> DT.UTCTime
stringToTime str =
  let parsedTime :: Either String DT.UTCTime =
        DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-dT%H:%M:%S%Q%Z" str
      extractedTime = fromRight parsedTime
  in  extractedTime

stringToDate :: String -> DT.UTCTime
stringToDate = fromJust . DT.parseTimeM True DT.defaultTimeLocale "%-m/%-d/%Y"

customAesonOptions :: A.Options
customAesonOptions = A.defaultOptions
  { A.sumEncoding            = sumOptions
  , A.constructorTagModifier = fmap C.toLower
  , A.fieldLabelModifier     = fmap C.toLower . P.drop 3
  }
 where
  sumOptions =
    A.TaggedObject { A.tagFieldName = "kind", A.contentsFieldName = "body" }

b64URLToB64 :: T.Text -> T.Text
b64URLToB64 = padder . T.replace "-" "+" . T.replace "_" "/"
 where
  paddingNeeded t = T.length t `mod` 4
  padder t = T.concat (t : replicate (paddingNeeded t) "=")

b64toB64URL :: T.Text -> T.Text
b64toB64URL = T.replace "=" "" . T.replace "+" "-" . T.replace "/" "_"

isAURL :: String -> Bool
isAURL s = "http" `isPrefixOf` s
