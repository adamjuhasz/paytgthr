{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}

module Shared.WebAPI.ApiDwolla.API
  ( module Shared.WebAPI.ApiDwolla.API
  , Routes(..)
  , TraceContext(..)
  , traceToMID
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Servant
import           Servant.API.Generic            ( Generic
                                                , GenericMode(..)
                                                )
import           Shared.Models.Payment          ( PaymentId )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..)
                                                , TraceHeaders
                                                , traceToMID
                                                )

type ABARouting = Text
type DDANumber = Text

data CreateFSBody = CreateFSBody
  { abaRoutingNo :: ABARouting
  , ddaAccountNo :: DDANumber
  , accountName  :: Text
  , bankName     :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON CreateFSBody
instance FromJSON CreateFSBody

newtype CreateFSResponse = CreateFSResponse
  { dwollaFSId :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON CreateFSResponse
instance FromJSON CreateFSResponse

data CreateDwollaAccountResponse = CreateDwollaAccountResponse {}
  deriving (Eq, Show, Generic)
instance ToJSON CreateDwollaAccountResponse
instance FromJSON CreateDwollaAccountResponse

newtype RemoveFSBody = RemoveFSBody
  { fsId :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON RemoveFSBody
instance FromJSON RemoveFSBody

-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  -- /user/:uid/
  { _AccountCreate       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "account" :> Post '[JSON] CreateDwollaAccountResponse
  , _FundingSourceCreate :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "fundingsource" :> ReqBody '[JSON] CreateFSBody :> Post '[JSON] CreateFSResponse
  , _FundingSourceRemove :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "fundingsource" :> ReqBody '[JSON] RemoveFSBody :> Delete '[JSON] NoContent

  -- /payment/:pid
  , _PaymentInitiate     :: route :- TraceHeaders :> "payment" :> Capture "paymentid" PaymentId :> Post '[JSON] NoContent
  , _PaymentCancel       :: route :- TraceHeaders :> "payment" :> Capture "paymentid" PaymentId :> "actions" :> "cancel" :> Post '[JSON] NoContent

  -- System
  , _health              :: route :- "_health" :> Get '[PlainText] Text
  }
  deriving Generic
