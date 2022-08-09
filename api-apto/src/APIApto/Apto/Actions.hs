{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{- HLINT ignore "Use newtype instead of data" -}

module APIApto.Apto.Actions where

import           APIApto.Apto.Client            ( Requester
                                                , RequesterError(..)
                                                )
import           Data.Aeson                     ( Object
                                                , ToJSON(toJSON)
                                                , Value(Object)
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as BL
import           Data.Coerce                    ( coerce )
import           Data.Functor                   ( (<&>) )
import           Data.HashMap.Strict            ( empty )
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           GHC.Stack                      ( HasCallStack )
import           Network.HTTP.Types.Method      ( StdMethod(..) )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..)
                                                , AptoErrorResponse
                                                )
import           Shared.Models.Apto.Card        ( AptoCard(..)
                                                , AptoCardChangePinRequest(..)
                                                , AptoCardCreateRequest(..)
                                                , AptoCardResponse(..)
                                                , CardLastFour
                                                )
import           Shared.Models.Apto.Cardholder  ( AptoAddress(..)
                                                , AptoCardholderRequest(..)
                                                , AptoCardholderResponse(..)
                                                )
import           Shared.Models.Card             ( AptoCardId(..)
                                                , CardDesign(..)
                                                , CardPin(CardPin)
                                                , CardPinEnc(..)
                                                )
import           Shared.Models.User             ( PhoneNumber(..)
                                                , RedactedText(RedactedText)
                                                , UserID
                                                , UserModel(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID )
import           Shared.Vault                   ( CipherText(..)
                                                , PlainText(..)
                                                )
import           System.Random                  ( getStdGen )
import           System.Random.Shuffle          ( shuffle' )

data ActionError
  = RequestFailure RequesterError
  | LastFourIncorrect CardLastFour
  deriving (Show)

toObject :: HasCallStack => Value -> Object
toObject (Object o) = o
toObject _          = error "Doesn't encode to object"

makeObject :: ToJSON a => a -> Object
makeObject = toObject . toJSON

decodeCardResponse
  :: Either RequesterError BL.ByteString -> Either ActionError AptoCardResponse
decodeCardResponse (Left  t) = Left $ RequestFailure t
decodeCardResponse (Right j) = case eitherDecode j of
  Left  s   -> Left . RequestFailure $ DecodeError s
  Right obj -> Right obj

decodeCard
  :: Either RequesterError BL.ByteString -> Either ActionError AptoCard
decodeCard (Left  t) = Left $ RequestFailure t
decodeCard (Right j) = case eitherDecode j of
  Left  s   -> Left . RequestFailure $ DecodeError s
  Right obj -> Right obj

decodeCardholder
  :: BL.ByteString -> Either RequesterError AptoCardholderResponse
decodeCardholder j = case eitherDecode j of
  Left  s   -> Left $ DecodeError s
  Right obj -> Right obj

decodeCardholderResponse
  :: Either RequesterError BL.ByteString
  -> Either ActionError AptoCardholderResponse
decodeCardholderResponse (Left  t) = Left $ RequestFailure t
decodeCardholderResponse (Right j) = case decodeCardholder j of
  Left  e        -> Left $ RequestFailure e
  Right response -> Right response

type SSNDecoder = (CipherText -> IO PlainText)
createCardholder
  :: SSNDecoder
  -> Requester
  -> MessageID
  -> UserModel
  -> IO (Either ActionError AptoCardholderResponse)
createCardholder decrypter client mid model = do
  (PlainText decryptedSSN) <- decrypter . coerce . fromJust $ usrSSN model
  let phoneNumber (PhoneNumber p) = "+1" <> p

  let
    req = AptoCardholderRequest
      { accrTgthrId     = usrUserID model
      , accrNameFirst   = fromJust $ usrFirstName model
      , accrNameLast    = fromJust $ usrLastName model
      , accrEmail       = usrEmail model
      , accrDOB         = fromJust $ usrDOB model
      , accrPhoneNumber = phoneNumber $ fromJust (usrPhone model)
      , accrAddress     = AptoAddress
                            { addStreetOne  = fromJust $ usrAddressStreet model
                            , addStreetTwo  = usrAddressStreet2 model
                            , addLocality   = fromJust $ usrAddressCity model
                            , addRegion     = fromJust $ usrAddressState model
                            , addPostalCode = fromJust $ usrAddressZip model
                            , addCountry    = "USA"
                            }
      , accrSSN         = decryptedSSN
      , accrCardDesign  = YellowToPink
      }
  res <- client POST "/cardholders" (makeObject req)
  let parsedRes = res >>= decodeCardholder
  case parsedRes of
    Left (StatusError status body) -> do
      putStr "Error in createCardholder: (parsedRes) "
        >> print (usrUserID model, mid, status, decodeError body)
      return $ Left $ RequestFailure $ StatusError status body
    Left err -> do
      putStr "Error in createCardholder: (parsedRes) "
        >> print (usrUserID model, mid, err)
      return $ Left $ RequestFailure err
    Right j -> return $ Right j
 where
  decodeError :: BL.ByteString -> Either String AptoErrorResponse
  decodeError = eitherDecode

type CardTuple = (UserID, AptoCardId, CardDesign)
createCard :: Requester -> [UserModel] -> IO (Either ActionError [CardTuple])
createCard _      []    = return $ Right []
createCard client users = do
  stdGen <- getStdGen
  let designs  = cycle [YellowToPink, PinkToYellow]
  let shuffled = shuffle' users (length users) stdGen
  let zipped   = userBuilder <$> zip shuffled designs

  cards <- mapM createACard zipped

  let userIDs = usrUserID <$> users
  putStr "createCard " >> print (userIDs, zipped, cards)

  return $ foldr folder (Right []) cards
 where
  cardRequest d = AptoCardCreateRequest { acdrDesign = d }
  createACard
    :: (AptoCardholderId, UserID, CardDesign)
    -> IO (Either ActionError CardTuple)
  createACard (AptoCardholderId ch, u, d) =
    client POST
           ("/cardholders/" <> T.unpack ch <> "/cards")
           (makeObject $ cardRequest d)
      <&> (\j -> decodeCardResponse j <&> acdxId . head . acrxCards <&> (u, , d)
          )
  userBuilder
    :: (UserModel, CardDesign) -> (AptoCardholderId, UserID, CardDesign)
  userBuilder (u, d) = (fromJust . usrAptoCardholderID $ u, usrUserID u, d)
  folder
    :: Either ActionError CardTuple
    -> Either ActionError [CardTuple]
    -> Either ActionError [CardTuple]
  folder _         (Left e)     = Left e
  folder (Left  e) _            = Left e
  folder (Right t) (Right list) = Right $ t : list

activateCard
  :: Requester -> AptoCardId -> CardLastFour -> IO (Either ActionError AptoCard)
activateCard client (AptoCardId cid) lastFour = do
  cardInfo <- client GET ("/cards/" <> T.unpack cid) empty
  putStr "cardInfo: " >> print (cid, cardInfo)
  let verified = verifyFour $ decodeCard cardInfo
  case verified of
    Left e -> return $ Left e
    Right _ ->
      client PUT ("/cards/" <> T.unpack cid <> "/ACTIVATE") empty <&> decodeCard
 where
  verifyFour :: Either ActionError AptoCard -> Either ActionError ()
  verifyFour (Left  e            ) = Left e
  verifyFour (Right AptoCard {..}) = if acdxLastFour /= lastFour
    then Left (LastFourIncorrect acdxLastFour)
    else Right ()

getCard :: Requester -> AptoCardId -> IO (Either ActionError AptoCard)
getCard client (AptoCardId cid) = do
  cardInfo <- client GET ("/cards/" <> T.unpack cid) empty
  return $ decodeCard cardInfo

type PinDecoder = CipherText -> IO PlainText
changePin
  :: PinDecoder
  -> Requester
  -> AptoCardId
  -> CardPinEnc
  -> IO (Either ActionError ())
changePin decrypter client c@(AptoCardId cid) pinEnc = do
  pin      <- coerce <$> decrypter (coerce pinEnc)
  cardInfo <- decodeCard <$> client
    PUT
    ("/cards/" <> T.unpack cid <> "/pin")
    (makeObject $ AptoCardChangePinRequest c pin)
  case cardInfo of
    Left  e -> return $ Left e
    Right r -> Right <$> (putStr "changePin response: " >> print (c, r))

closeCard :: Requester -> AptoCardId -> IO (Either ActionError AptoCard)
closeCard client (AptoCardId cid) =
  decodeCard <$> client PUT ("/cards/" <> T.unpack cid <> "/CLOSE") empty

getCardholder
  :: Requester
  -> AptoCardholderId
  -> IO (Either ActionError AptoCardholderResponse)
getCardholder client (AptoCardholderId cid) =
  decodeCardholderResponse
    <$> client GET ("/cardholders/" <> T.unpack cid) empty
