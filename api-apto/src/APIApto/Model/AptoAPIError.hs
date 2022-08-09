module APIApto.Model.AptoAPIError where

import           Data.Aeson
import           Data.Text                      ( Text )

-- # OfflinePINOffset Error definition
-- The PIN has been successfully reset from the web service which returned the response code: 00. However, the PIN needs to be updated on the physical card. The cardholder has to physically go to a terminal having PIN Writing capability and perform the transaction so, the updated PIN will be writen on the card.
-- Whenever the cardholder will perform the first transaction after PIN reset it may get a decline as this transaction will update the new PIN on the card. Afterward, the cardholder can use the updated PIN for the transaction which will be successful.
-- Please note that the cardholder can't reset PIN two times at once. In your case the first PIN got successful but after the first PIN the cardholder has to go to the physical terminal so, the PIN can be written. However, the cardholder hasn't visited the physical terminal having PIN Writing capability for PIN updation after the first reset which is why the second PIN reset transaction got declined.

data InvalidRequest
  = OfflinePINOffset
  | UnknownInvalid Text
  deriving (Eq, Show)

data AptoAPIError
  = InvalidRequestError InvalidRequest
  | TemporaryAPIError
  | UnknownAPIError Text
  deriving (Eq, Show)

instance FromJSON AptoAPIError where
  parseJSON = withObject "AptoAPIError" $ \o -> do
    errorObj               <- o .: "error"
    errorType :: Text      <- errorObj .: "type"
    errorCode :: Maybe Int <- errorObj .: "error_code"
    errorMsg :: Text       <- errorObj .: "message"

    case (errorType, errorCode) of
      ("invalid_request_error", Just 200046) ->
        return $ InvalidRequestError OfflinePINOffset
      ("invalid_request_error", _) ->
        return . InvalidRequestError $ UnknownInvalid errorMsg
      ("api_error", _) -> return TemporaryAPIError
      (_          , _) -> return $ UnknownAPIError errorMsg
