-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, StrictData #-}

module Shared.TgthrMessages.Apto where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Apto.Base        ( AptoCardholderId )
import           Shared.Models.Card             ( AptoCardId
                                                , CardDesign
                                                , CardLastFour
                                                , CardPinEnc
                                                , CardStatus
                                                )
import           Shared.Models.KYC
import           Shared.Models.User             ( UserID )
import           Shared.TgthrMessages.Base      ( ThroughMQ(..) )
import           Shared.Utils                   ( customAesonOptions )

data AptoCmd
  -- /user/:uid/card/action/activate
  = ActivateCard
    { accUser     :: UserID
    , accLastFour :: CardLastFour
    }
  -- /user/:uid/card/pin
  | ChangeCardPin
    { cccUser   :: UserID
    , cccPinEnc :: CardPinEnc
    }
  -- /user/:uid/card/lastfour
  | GetLastFour
    { lfcUser :: UserID
    }
  -- /user/:uid/cardholder
  | CreateCardholder
    { cccUser :: UserID
    }
  -- /user/:uid/card/action/close
  | CloseCurrentCard
    { xccUser :: UserID
    }
  -- /user/:uid/card
  | CreateCard
    { cxcUser :: UserID
    }
  -- /admin/user/:uid/sync/pull
  | SyncUserWithApto
    { sacUser :: UserID
    }
  deriving (Eq, Show, Generic)

instance FromJSON AptoCmd where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AptoCmd where
  toJSON = genericToJSON customAesonOptions
instance ThroughMQ AptoCmd where
  toKey ActivateCard{}     = "apto.cmd.activatecard"
  toKey ChangeCardPin{}    = "apto.cmd.changepin"
  toKey GetLastFour{}      = "apto.cmd.getlastfour"
  toKey CreateCardholder{} = "apto.cmd.createcardholder"
  toKey CloseCurrentCard{} = "apto.cmd.closecurrentcard"
  toKey CreateCard{}       = "apto.cmd.createcard"
  toKey SyncUserWithApto{} = "apto.cmd.syncuserwithapto"

data AptoEvent
  = AptoCardholderCreated
    { cheUser :: UserID
    , cheCardHolder :: AptoCardholderId
    }
  | AptoKYCUpdated
    { akeUser :: UserID
    , akeKYCStatus :: KycStatus
    }
  | CardStateChanged
    { caeUser :: UserID
    , caeCard :: AptoCardId
    , caeState :: CardStatus
    , caeDesign :: CardDesign
    }
  deriving (Eq, Show, Generic)
instance FromJSON AptoEvent where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AptoEvent where
  toJSON = genericToJSON customAesonOptions
instance ThroughMQ AptoEvent where
  toKey AptoCardholderCreated{} = "apto.event.cardholdercreated"
  toKey AptoKYCUpdated{}        = "apto.event.kycupdate"
  toKey CardStateChanged{}      = "apto.event.cardstatechange"

data AptoReplies
  = GetLastFourReply
    { lfrUser :: UserID
    , lfrLastFour :: CardLastFour
    }
  | BlankAptoReplies {} -- needed for genericParseJSON
  deriving (Eq, Show, Generic)
instance FromJSON AptoReplies where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AptoReplies where
  toJSON = genericToJSON customAesonOptions
