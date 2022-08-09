{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Shared.TgthrMessages.Excpetions where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( genericParseJSON
                                                , genericToJSON
                                                , FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                )
import           Shared.Utils                   ( customAesonOptions )
import           Shared.TgthrMessages.Base      ( ThroughMQ(..) )

data Exception = Exception
  { excService :: Text
  , excMsg :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Exception where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON Exception where
  toJSON = genericToJSON customAesonOptions
instance ThroughMQ Exception where
  toKey Exception{} = "exception.general"
