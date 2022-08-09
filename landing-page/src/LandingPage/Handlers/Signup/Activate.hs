
module LandingPage.Handlers.Signup.Activate where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( unless )
import           Data.Aeson                     ( Value
                                                , object
                                                , KeyValue((.=))
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Vault.Lazy               as V
import           Network.Wai                    ( Request(vault) )
import           Network.HTTP.Types.Status      ( status400
                                                , status500
                                                )
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

import           Shared.Amqp                    ( CommandMessage(AptoCmd)
                                                , MessageBody(ReplyV1)
                                                , ReplyMessage(ReplySuccessV1)
                                                , ReplySuccess(CmdSuccess)
                                                , AMQPPublisher
                                                )
import           Shared.Models.Card             ( CardLastFour(CardLastFour)
                                                , CardPinEnc(CardPinEnc)
                                                )
import           Shared.TgthrMessages.Apto      ( AptoCmd(..) )
import           Shared.Vault                   ( PlainText(..) )
import           LandingPage.Types             as Types
                                                ( EncryptedPin(..)
                                                , SessionData
                                                )
import           LandingPage.Utils              ( expectSession
                                                , getAcceptType
                                                , requireUserNotClosed
                                                , AcceptType(..)
                                                )
import           LandingPage.Validators         ( isBothEqual
                                                , isFixedLengthNumber
                                                )

renderCardActivate :: [(Text, Value)] -> ActionM ()
renderCardActivate inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object inParams
    TextHTML        -> Scotty.next

cardActiveRenderer :: [(Text, Value)] -> ActionM ()
cardActiveRenderer inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object inParams
    TextHTML        -> Scotty.next

handleCardActivate
  :: (PlainText -> IO EncryptedPin)
  -> V.Key SessionData
  -> AMQPPublisher
  -> ActionM ()
handleCardActivate encrypter sKey pub = do
  four       <- param "last-four"
  pin        <- param "pin"
  pinConfirm <- param "pin-confirm"
  user       <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosed pub

  let zipped =
        [ (isFixedLengthNumber 4 four, "fourError" .= True)
        , (isFixedLengthNumber 4 pin , "pinLength" .= True)
        , (isBothEqual pin pinConfirm, "pinEqual" .= True)
        ]
  let folded =
        foldr (\(m, e) acc -> if isNothing m then e : acc else acc) [] zipped

  unless (null folded) (status status400 >> rejectInput folded >> finish)

  let activateCard = AptoCmd $ ActivateCard user (CardLastFour four)

  activationGood <- liftAndCatchIO $ pub activateCard

  liftAndCatchIO $ putStr "handleCardActivate ActivateCard " >> print
    (user, isSuccess activationGood, activationGood)

  case isSuccess activationGood of
    Right _ -> return ()
    Left  _ -> status status400 >> rejectInput ["fourError" .= True] >> finish

  liftAndCatchIO $ threadDelay 2000000 -- 2 seconds for eventual consistency

  pinGood <-
    liftAndCatchIO
    $   encrypter (PlainText pin)
    <&> coerce
    >>= (pub . AptoCmd . ChangeCardPin user)

  liftAndCatchIO $ putStr "handleCardActivate ChangeCardPin " >> print
    (user, isSuccess pinGood, pinGood)

  case isSuccess pinGood of
    Right _ -> return ()
    Left  _ -> do
      liftAndCatchIO
        $  putStr "Error: handleCardActivate ChangeCardPin "
        >> print (user, pinGood)
      status status500 >> rejectInput [] >> finish

  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object ["success" .= True]
    TextHTML        -> Scotty.next
 where
  rejectInput = renderCardActivate
  isSuccess (ReplyV1 (ReplySuccessV1 CmdSuccess)) = Right ()
  isSuccess _ = Left ("Not a success reply" :: Text)
