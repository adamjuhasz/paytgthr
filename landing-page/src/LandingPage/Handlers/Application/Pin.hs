{- HLINT ignore "Redundant <&>" -}

module LandingPage.Handlers.Application.Pin where

import           Control.Monad                  ( unless )
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isNothing )
import           Data.UUID                      ( fromText )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( EncryptedPin(..)
                                                , SessionData
                                                )
import           LandingPage.Utils              ( AcceptType(..)
                                                , createTrace
                                                , expectSession
                                                , getAcceptType
                                                , requireUserNotClosedHTTP
                                                )
import           LandingPage.Validators         ( isFixedLengthNumber )
import           Network.HTTP.Types.Status      ( status400
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant                        ( NoContent(..) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Card             ( CardPinEnc(CardPinEnc) )
import           Shared.Models.Ids              ( CardId(CardId) )
import           Shared.Vault                   ( PlainText(..) )
import           Shared.WebAPI.AccountsFSM.API  ( ChangePinBody(..)
                                                , Routes(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( asClientM )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , next
                                                , param
                                                , request
                                                , status
                                                )

pinChangeHandler
  :: (PlainText -> IO EncryptedPin)
  -> V.Key SessionData
  -> ClientEnv
  -> ActionM ()
pinChangeHandler encrypter sKey acountsEnv = do
  (user, _) <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosedHTTP acountsEnv

  pin         <- param "pin"
  cardIdMaybe <- fromText <$> param "cardid"

  let zipped = [(isFixedLengthNumber 4 pin, "pinLength" .= True)]
  let folded =
        foldr (\(m, e) acc -> if isNothing m then e : acc else acc) [] zipped
  unless (null folded)
         (status status400 >> Scotty.json (object folded) >> finish)

  cardId <- case cardIdMaybe of
    Nothing -> status status400 >> finish
    Just u  -> return $ CardId u

  trace                       <- createTrace

  (EncryptedPin encryptedPin) <- liftAndCatchIO $ encrypter (PlainText pin)
  let body = ChangePinBody { newPinText      = pin
                           , newPinEncrypted = CardPinEnc encryptedPin
                           }
  let changePin = _UserSetCardPin asClientM trace user cardId body

  changeRes <- liftAndCatchIO $ runClientM changePin acountsEnv
  case changeRes of
    Right NoContent -> do
      accepts <- getAcceptType
      case accepts of
        ApplicationJSON -> Scotty.json $ object ["success" .= True]
        TextHTML        -> Scotty.next
    Left e -> do
      liftAndCatchIO $ putStr "Error: Setting pin didn't work " >> print
        (user, cardId, e)
      status status500 >> Scotty.json (object ["success" .= False])
