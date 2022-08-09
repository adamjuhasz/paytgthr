{-# LANGUAGE RecordWildCards #-}

module AFSM.Web.User.Query.GetUsersByTrait where

import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           Control.Monad.IO.Class
import           Data.Maybe                     ( catMaybes
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Card             ( IssuerPlatform )
import           Shared.Models.Cardholder       ( CardholderId )
import           Shared.Models.User             ( EmailAddress
                                                , PhoneNumber
                                                , UserModel
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( QueryBankFSBody(..)
                                                , TraceContext
                                                )
getUsersByCardholder
  :: (HasGetUserDB m) => TraceContext -> CardholderId -> m [UserModel]
getUsersByCardholder _ cardholder =
  maybeToList <$> getUserByCardholder cardholder

getUserByCardId
  :: (HasGetUserDB m) => TraceContext -> IssuerPlatform -> m [UserModel]
getUserByCardId trace cardid = maybeToList <$> getUserByCard trace cardid

getUserByPhoneNumber
  :: (HasGetUserDB m) => TraceContext -> PhoneNumber -> m [UserModel]
getUserByPhoneNumber _ phone = maybeToList <$> getUserByPhone phone

getUserByEmailAddress
  :: (HasGetUserDB m) => TraceContext -> EmailAddress -> m [UserModel]
getUserByEmailAddress _ email = maybeToList <$> getUserByEmail email

getUserByBankNumbers
  :: (HasGetUserDB m, MonadIO m)
  => TraceContext
  -> QueryBankFSBody
  -> m [UserModel]
getUserByBankNumbers trace QueryBankFSBody {..} = do
  userIds <- getUserByBank routingNumber accountNumber
  tracePrint trace
             "getUserByBankNumbers "
             (routingNumber, accountNumber, userIds)
  catMaybes <$> mapM getUserById userIds

getUserByEncryptedSSN
  :: (HasGetUserDB m) => TraceContext -> Text -> m [UserModel]
getUserByEncryptedSSN _ ssn = do
  userIds <- getUsersWithSSN ssn
  catMaybes <$> mapM getUserById userIds
