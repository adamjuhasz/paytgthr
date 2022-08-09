{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module APIPrivacy.InternalAPI.CardholderCreate where

import           APIPrivacy.Monad.HasAccounts   ( HasAccounts(..) )
import           APIPrivacy.Monad.HasDecryption ( HasDecryption(..) )
import           APIPrivacy.Monad.HasPrivacyClient
                                                ( HasPrivacyClient(..) )
import           APIPrivacy.PrivacyClient       ( EnrollUserBody(..)
                                                , EnrollUserResponse(..)
                                                , Routes(..)
                                                , asClientM
                                                )
import           Control.Exception              ( SomeException )
import           Control.Monad.Catch            ( MonadCatch(catch) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy.Char8    as C
import qualified Data.Text                     as T
import           Servant                        ( ServerError(..)
                                                , err404
                                                , err500
                                                )
import           Shared.Console
import           Shared.Models.Cardholder       ( CardholderId(..) )
import           Shared.Models.User             ( RedactedText(..)
                                                , UserID
                                                , UserModel(..)
                                                )
import           Shared.Vault                   ( CipherText(..)
                                                , PlainText(..)
                                                )
import           Shared.WebAPI.ApiPrivacy.API   ( CreateCardholderAction(..)
                                                , TraceContext
                                                )

extractUserElem :: (MonadError ServerError m) => (a -> Maybe b) -> a -> m b
extractUserElem fn user = case fn user of
  Nothing -> throwError err500
  Just t  -> return t

createCardholder
  :: ( MonadIO m
     , HasPrivacyClient m
     , HasAccounts m
     , HasDecryption m
     , MonadError ServerError m
     , MonadCatch m
     )
  => TraceContext
  -> UserID
  -> m CreateCardholderAction
createCardholder trace userid = do
  userMaybe <- getUser trace userid
  user      <- case userMaybe of
    Nothing -> throwError err404
    Just u  -> return u

  firstName <- extractUserElem usrFirstName user
  lastName  <- extractUserElem usrLastName user
  dob       <- extractUserElem usrDOB user
  street1   <- extractUserElem usrAddressStreet user
  let street2 = usrAddressStreet2 user
  zipCode     <- extractUserElem usrAddressZip user
  phoneNumber <- extractUserElem usrPhone user -- Add +1 to the beginning
  let email = usrEmail user
  RedactedText encryptedSSN <- extractUserElem usrSSN user
  decryptResult             <-
    (Right <$> decryptSSN trace (CipherText encryptedSSN))
      `catch` (\(e :: SomeException) -> return $ Left e)
  PlainText plainSSN <- case decryptResult of
    Right p -> return p
    Left  e -> do
      traceError trace
                 "Error: Could not decrypt user's SSN "
                 (userid, encryptedSSN, e)
      error $ "Could not decrypt " <> show (userid, encryptedSSN, e)

  let enrollee = EnrollUserBody
        { firstName
        , lastName
        , dob
        , street1
        , street2
        , zipCode
        , ssnLastFour = RedactedText $ T.takeEnd 4 plainSSN
        , phoneNumber
        , email
        }

  tracePrint trace "createCardholder enrollee: " enrollee

  auth <- getPrivacyAuth
  res  <- runPrivacyCmd $ _EnrollUser asClientM auth enrollee
  tracePrint trace "createCardholder _EnrollUser res: " (userid, res)

  token <- case res of
    Left e -> do
      traceError trace
                 "Error: createCardholder _EnrollUser "
                 (userid, e, encode enrollee)
      throwError err500
        { errBody = C.pack $ "Error: _EnrollUser " <> show (userid, e)
        }
    Right EnrollUserResponse {..} -> return accountToken

  tracePrint trace "_EnrollUser token: " (userid, token)

  return $ CardholderCreated $ PayWithPrivacyCH token
