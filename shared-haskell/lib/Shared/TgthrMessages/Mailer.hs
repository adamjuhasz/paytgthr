{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, StrictData #-}

module Shared.TgthrMessages.Mailer where

---------------------------------------------------------------
import           Data.Aeson                     ( genericParseJSON
                                                , genericToJSON
                                                , FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Shared.Utils                   ( customAesonOptions )
import           Shared.Models.User             ( UserID )
import           Shared.TgthrMessages.Base      ( ThroughMQ(..) )
---------------------------------------------------------------

data MailerEvent
  = AccountCreatedSent
    { wmeUser :: UserID
    , wmeMessage :: Text
    }
  | InviteSent
    { imeUser :: UserID
    , imeMessage :: Text
    }
  | AccountActivatedSent
    { ameUser :: UserID
    , ameMessage :: Text
    }
  | CardCreatedSent
    { cmeUser :: UserID
    , cmeMessage :: Text
    }
  | CardActivatedSent
    { xmeUser :: UserID
    , xmeMessage :: Text
    }
  | SelfKYCFailSent
    { smeUser :: UserID
    , smeMessage :: Text
    }
  | PartnerKYCFailSent
    { pmeUser :: UserID
    , pmeMessage :: Text
    }
  | TransactionAuthedSent
    { taeUser :: UserID
    , taeMessage :: Text
    }
  | TransactionDeclinedSent
    { tdeUser :: UserID
    , tdeMessage :: Text
    }
  | PaymentPendingSent
    { ppeUser :: UserID
    , ppeMessage :: Text
    }
  | PaymentCompleteSent
    { pceUser :: UserID
    , pceMessage :: Text
    }
  | PaymentFailedSent
    { pfeUser :: UserID
    , pfeMessage :: Text
    }
  | ResetPasswordSent
    { rpeUser :: UserID
    , rpeMessage :: Text
    }
  | ForceLinkSent
    { fceUser :: UserID
    , fceMessage :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON MailerEvent where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON MailerEvent where
  toJSON = genericToJSON customAesonOptions
instance ThroughMQ MailerEvent where
  toKey AccountCreatedSent{}      = "mailer.event.usercreatedsent"
  toKey InviteSent{}              = "mailer.event.invitesent"
  toKey CardCreatedSent{}         = "mailer.event.cardcreatedsent"
  toKey CardActivatedSent{}       = "mailer.event.cardactivatedsent"
  toKey AccountActivatedSent{}    = "mailer.event.useractivesent"
  toKey SelfKYCFailSent{}         = "mailer.event.selfkycfailsent"
  toKey PartnerKYCFailSent{}      = "mailer.event.partnerkycfailsent"
  toKey TransactionAuthedSent{}   = "mailer.event.transactionauthsent"
  toKey TransactionDeclinedSent{} = "mailer.event.transactiondeclinedsent"
  toKey PaymentPendingSent{}      = "mailer.event.paymentpendingsent"
  toKey PaymentCompleteSent{}     = "mailer.event.paymentcompletesent"
  toKey PaymentFailedSent{}       = "mailer.event.paymentfailedsent"
  toKey ResetPasswordSent{}       = "mailer.event.passwordresetsent"
  toKey ForceLinkSent{}           = "mailer.event.forcelinksent"
