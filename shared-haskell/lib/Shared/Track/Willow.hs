{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, RecordWildCards, TypeOperators #-}

module Shared.Track.Willow where

import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value
                                                , object
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.UUID                      ( nil )
import           Network.HTTP.Client.TLS        ( newTlsManager )
import           Servant.API
import           Servant.API.Generic            ( Generic
                                                , GenericMode(..)
                                                )
import           Servant.Client                 ( ClientM
                                                , mkClientEnv
                                                , parseBaseUrl
                                                , runClientM
                                                )
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                )
import           Shared.Models.Ids              ( UserID(UserID) )

data AttributeUpdate = AttributeUpdate
  { category   :: Text
  , objectId   :: Text
  , attributes :: Value
  , related    :: [(Text, Text)]
  }
  deriving (Eq, Show)
instance ToJSON AttributeUpdate where
  toJSON AttributeUpdate {..} = object
    [ "category" .= category
    , "id" .= objectId
    , "attributes" .= attributes
    , "related" .= map (\(c, i) -> object ["category" .= c, "id" .= i]) related
    ]

newtype Routes route = Routes
  { _UpdateAttribute
      :: route :- Header' '[Required , Strict] "Authorization" Text :> "api" :> "v1" :> "attribute" :> ReqBody '[JSON] AttributeUpdate :> Post '[JSON] NoContent
  }
  deriving Generic

willowClientM :: Routes (AsClientT ClientM)
willowClientM = genericClient

trackExample :: IO ()
trackExample = do
  externalTLSManager <- newTlsManager
  willowEnv          <- mkClientEnv externalTLSManager
    <$> parseBaseUrl "https://willow.support"

  let fn = _UpdateAttribute
        willowClientM
        "Bearer aaaa"
        (AttributeUpdate "cat"
                         "id_1"
                         (object [])
                         [("user", T.pack . show $ UserID nil)]
        )
  res <- runClientM fn willowEnv
  print res

  return ()
