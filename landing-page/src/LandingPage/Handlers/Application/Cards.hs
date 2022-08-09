{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Reduce duplication" -}

module LandingPage.Handlers.Application.Cards where

import           Control.Monad                  ( when )
import           Data.Aeson                     ( object )
import           Data.Functor                   ( (<&>) )
import           Data.UUID                      ( fromText )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                , requireUserNotClosedHTTP
                                                )
import           Network.HTTP.Types             ( status400
                                                , status401
                                                , status403
                                                , status404
                                                , status500
                                                , status503
                                                )
import           Network.HTTP.Types.Status      ( Status(Status, statusCode) )
import           Network.Wai                    ( Request(vault) )
import           Servant.API                    ( NoContent(NoContent) )
import           Servant.Client                 ( ClientEnv
                                                , ClientError(FailureResponse)
                                                , ResponseF
                                                  ( Response
                                                  , responseStatusCode
                                                  )
                                                , runClientM
                                                )
import           Shared.Models.Card             ( CardLastFour(CardLastFour)
                                                , CardModel
                                                  ( CardModel
                                                  , cardPlatform
                                                  , cardholder
                                                  )
                                                , CardStatus
                                                  ( CardActive
                                                  , CardUserFrozen
                                                  )
                                                , IssuerPlatform
                                                  ( AptoPayments
                                                  , PayWithPrivacy
                                                  )
                                                )
import           Shared.Models.Ids              ( CardId(CardId)
                                                , UserID
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( ChangeCardStateBody(..)
                                                , Routes(..)
                                                , accountsClientM
                                                , asClientM
                                                )
import           Shared.WebAPI.ApiPrivacy.Client
                                                ( Routes(_CardPCIInfo)
                                                , pricacyClientM
                                                )
import           Shared.WebAPI.General.Issuer   ( CardActivateBody(..) )
import           Text.Read                      ( readMaybe )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , param
                                                , request
                                                , status
                                                , text
                                                )

getUserFromSession :: V.Key SessionData -> ActionM UserID
getUserFromSession sKey =
  request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser

getUserCards :: V.Key SessionData -> ClientEnv -> ActionM ()
getUserCards sKey accountsEnv = do
  user  <- getUserFromSession sKey
  trace <- createTrace

  let fn = _UserCardList accountsClientM trace user
  res <- liftAndCatchIO $ runClientM fn accountsEnv
  case res of
    Right cards -> Scotty.json cards
    Left  e     -> do
      liftAndCatchIO $ putStr "Error: getUserCards _UserCardList " >> print
        (user, e, trace)
      status status500

getCardInfo :: V.Key SessionData -> ClientEnv -> ClientEnv -> ActionM ()
getCardInfo sKey accountsEnv privacyEnv = do
  user          <- getUserFromSession sKey
  requestedCard <- fromText <$> param "cardid"
  trace         <- createTrace

  givenCardId   <- case requestedCard of
    Nothing   -> status status400 >> text "Bad ID" >> finish
    Just uuid -> return $ CardId uuid

  let getCardFn = _GetCard asClientM trace givenCardId
  getCardRes <- liftAndCatchIO $ runClientM getCardFn accountsEnv

  card       <- case getCardRes of
    Right (Just c) -> return c
    Right Nothing  -> status status404 >> finish
    Left  e        -> do
      liftAndCatchIO $ putStr "Error: getCardInfo _GetCard " >> print
        (user, e, trace)
      status status500 >> finish

  when (cardholder card /= user) (status status403 >> finish)

  token <- case card of
    CardModel { cardPlatform = AptoPayments _ } ->
      status status400 >> text "Apto" >> finish
    CardModel { cardPlatform = PayWithPrivacy token } -> return token

  let getCardinfo = _CardPCIInfo pricacyClientM trace user token
  getCardinfoRes <- liftAndCatchIO $ runClientM getCardinfo privacyEnv

  case getCardinfoRes of
    Right info -> Scotty.json info
    Left  e    -> do
      liftAndCatchIO $ putStr "Error: getCardInfo _CardPCIInfo " >> print
        (user, e, trace)
      status status500 >> finish

setCardState :: CardStatus -> V.Key SessionData -> ClientEnv -> ActionM ()
setCardState newState sKey accountsEnv = do
  user        <- getUserFromSession sKey
  requestedId <- fromText <$> param "cardid"
  trace       <- createTrace

  givenCardId <- case requestedId of
    Nothing   -> status status400 >> text "Bad ID" >> finish
    Just uuid -> return $ CardId uuid

  let getCardFn = _GetCard asClientM trace givenCardId
  getCardRes <- liftAndCatchIO $ runClientM getCardFn accountsEnv
  card       <- case getCardRes of
    Left e -> do
      liftAndCatchIO $ putStr "Error: setCardState _GetCard" >> print
        (givenCardId, e)
      status status500 >> finish
    Right Nothing  -> status status404 >> finish
    Right (Just c) -> return c

  when (cardholder card /= user) (status status403 >> finish)

  let lockCardFn =
        _UserSetCardState asClientM trace user givenCardId
          $ ChangeCardStateBody newState Nothing Nothing
  result <- liftAndCatchIO $ runClientM lockCardFn accountsEnv
  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 403 } })
      -> status status403 >> finish
    Left e -> do
      liftAndCatchIO $ putStr "Error: lockCard _UserSetCardState" >> print
        (givenCardId, e)
      status status500 >> finish
    Right _ -> Scotty.json $ object []

lockCard :: V.Key SessionData -> ClientEnv -> ActionM ()
lockCard = setCardState CardUserFrozen

unlockCard :: V.Key SessionData -> ClientEnv -> ActionM ()
unlockCard = setCardState CardActive

activateCard :: V.Key SessionData -> ClientEnv -> ActionM ()
activateCard sKey accountsEnv = do
  user        <- getUserFromSession sKey
  cardIdMaybe <- fromText <$> param "cardid"
  lastfour    <- param "lastFour"
  trace       <- createTrace

  cardId      <- case cardIdMaybe of
    Nothing -> status status401 >> finish
    Just uu -> return $ CardId uu

  let lockCardFn =
        _UserCardActivate asClientM trace user cardId
          $ CardActivateBody
          $ CardLastFour lastfour
  result <- liftAndCatchIO $ runClientM lockCardFn accountsEnv
  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 403 } })
      -> status status403 >> finish
    Left e -> do
      liftAndCatchIO $ putStr "Error: lockCard _UserSetCardState" >> print
        (user, cardId, lastfour, e)
      status status500 >> finish
    Right _ -> Scotty.json $ object []

createNewCard :: V.Key SessionData -> ClientEnv -> ActionM ()
createNewCard sKey accountsEnv = do
  (user, _) <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosedHTTP accountsEnv

  -- do not make cards
  _          <- status status503 >> Scotty.json (object []) >> finish

  cTypeMaybe <- readMaybe <$> param "type"
  cType      <- case cTypeMaybe of
    Nothing ->
      status status400
        >> Scotty.json (object [("error", "Bad card type")])
        >> finish
    Just c -> return c

  trace <- createTrace

  let fn = _UserCreateCard accountsClientM trace user cType
  res <- liftAndCatchIO $ runClientM fn accountsEnv
  case res of
    Right NoContent -> Scotty.json $ object []
    Left  e         -> do
      liftAndCatchIO $ putStr "Error: _UserCreateCard " >> print
        (user, cType, e)
      status status500 >> Scotty.json (object []) >> finish
