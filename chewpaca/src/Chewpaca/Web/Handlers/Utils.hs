module Chewpaca.Web.Handlers.Utils where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text
                                                , replace
                                                )
import           Data.UUID                      ( toText )
import           Data.UUID.V4                   ( nextRandom )

aptoAPIUrl :: String
aptoAPIUrl = "https://apiapto-web-internal.default.svc.cluster.local:443"

genTrace :: (MonadIO m) => m Text
genTrace = liftIO $ replace "-" "" . toText <$> nextRandom
