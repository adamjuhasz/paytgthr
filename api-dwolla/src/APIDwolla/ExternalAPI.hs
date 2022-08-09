{- HLINT ignore "Reduce duplication" -}

module APIDwolla.ExternalAPI where

import           APIDwolla.Dwolla.Client        ( ClientErrors
                                                , DwollaFailureReason(..)
                                                , TokenMaker
                                                , getFailureReason
                                                )
import           APIDwolla.Dwolla.Types         ( CustomerTopic(..)
                                                , DwollaEvent(..)
                                                , DwollaEventTopic(..)
                                                , TransferTopic(..)
                                                )
import           Control.Concurrent             ( MVar
                                                , tryReadMVar
                                                )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as TL
import           Network.HTTP.Types.Status      ( status500
                                                , status503
                                                )
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp       ( Port )
import           Servant.Client                 ( ClientEnv
                                                , ClientError
                                                , runClientM
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentFailureCode(ACHUnknown)
                                                , PaymentStatus(..)
                                                )
import           Shared.Models.User             ( UserModel(..) )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , accountsClientM
                                                , incrementTrace
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , parseTrace
                                                , randomTrace
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( Routes(..)
                                                , UpdatePaymentBody(..)
                                                , payAuthClientM
                                                )
import           Web.Scotty                     ( ActionM
                                                , defaultHandler
                                                , finish
                                                , get
                                                , header
                                                , jsonData
                                                , liftAndCatchIO
                                                , post
                                                , rescue
                                                , scottyApp
                                                , status
                                                , text
                                                )

createTrace :: ActionM TraceContext
createTrace = do
  traceMaybe <- header "X-Cloud-Trace-Context"
  case traceMaybe of
    Nothing -> randomTrace
    Just t  -> case parseTrace $ TL.toStrict t of
      Left e -> do
        liftAndCatchIO $ putStr "Error: X-Cloud-Trace-Context " >> print
          (traceMaybe, e)
        randomTrace
      Right trace -> incrementTrace trace

data AppSettings = AppSettings
  { inProd         :: Bool
  , port           :: Port
  , tokener        :: IO TokenMaker
  , payAuthEnv     :: ClientEnv
  , accountsEnv    :: ClientEnv
  , isShuttingDown :: MVar Bool
  }

apiApp :: AppSettings -> IO Application
apiApp AppSettings {..} = scottyApp $ do
  defaultHandler $ \err -> do
    trace <- createTrace
    liftAndCatchIO (traceError trace "Error: defaultHandler " err)
    status status500 >> text "500"

  get "/ping" $ do
    res <- liftAndCatchIO $ tryReadMVar isShuttingDown
    case res of
      Nothing    -> text "pong"
      Just False -> text "pong"
      Just True  -> do
        liftAndCatchIO $ putStrLn "SIGTERM received... returning 503 External"
        status status503 >> text "Not Ok"

  post "/webhook" $ handleWebhook tokener accountsEnv payAuthEnv

handleWebhook :: IO TokenMaker -> ClientEnv -> ClientEnv -> ActionM ()
handleWebhook tokener accountsEnv payAuthEnv = do
  incoming :: DwollaEvent <- jsonData
  trace                   <- createTrace
  tracePrint trace "Webhook: " incoming

  let updater = updatePayment trace accountsEnv payAuthEnv tokener incoming
  let
    action = case eventTopic incoming of
      Customer (TransferBank Completed) -> updater PaymentCompleted
      Customer (Transfer     Completed) -> updater PaymentCompleted
      Customer (TransferBank Failed) ->
        updater (PaymentFailed (ACHUnknown "Unknown"))
      Customer (Transfer Failed) ->
        updater (PaymentFailed (ACHUnknown "Unknown"))
      Customer (TransferBank CreationFailed) ->
        updater (PaymentFailed (ACHUnknown ""))
      Customer (Transfer CreationFailed) ->
        updater (PaymentFailed (ACHUnknown ""))
      -- Cancelled
      Customer (TransferBank Cancelled) -> updater PaymentCancelled
      Customer (Transfer Cancelled) -> updater PaymentCancelled
      v -> tracePrint trace "Warn: Unprocessed webhook: " v
  let catcher msg = do
        liftAndCatchIO $ traceError trace "Error: processing" (msg, incoming)
        status status500 >> text "Error: contact support hi@example.com"
        finish

  liftAndCatchIO action `rescue` catcher
  text "ok"

data UpdatePaymentError
  = CantFindPaymentFromSourceId Text
  | CantFindUser UserID
  | CantLinkUserToDwollaCustomer (Maybe Text)
  | CantUpdatePayment ClientError
  | CantGetFailureReason ClientErrors
  deriving (Show)
instance Exception UpdatePaymentError

updatePayment
  :: TraceContext
  -> ClientEnv
  -> ClientEnv
  -> IO TokenMaker
  -> DwollaEvent
  -> PaymentStatus
  -> IO ()
updatePayment trace accountsEnv payAuthEnv tokener evt newPayStatus = do
  let paymentId = linkResource evt
  let customer  = linkCustomer evt
  let nilCustomer =
        Just "https://mock/customers/00000000-0000-0000-0000-000000000000"
  let mcbCustomer =
        "https://api.dwolla.com/customers/00000000-0000-0000-0000-000000000000"

  if customer == Just mcbCustomer
    then return () -- This is our Pay Tgthr user, we only care about the other side
    else do
      let getPayment = _QueryPaymentSourceId payAuthClientM trace paymentId
      paymentEither <- runClientM getPayment payAuthEnv
      case paymentEither of
        Left e -> do
          traceError trace
                     "Error: _QueryPaymentSourceId "
                     (paymentId, customer, newPayStatus, e)
          throw $ CantFindPaymentFromSourceId paymentId
        Right payment@Payment {..} -> do
          let getUser = _UserGet accountsClientM trace payUser
          userMaybe <- runClientM getUser accountsEnv
          case userMaybe of
            Left e -> do
              traceError trace "Error: _UserGet " (payUser, payId, e)
              throw $ CantFindUser payUser
            Right UserModel {..} ->
              if usrDwollaId == customer || customer == nilCustomer
                then do
                  let pid = Just paymentId
                  let updateBody = UpdatePaymentBody
                        { paymentStatus      = newPayStatus
                        , paymentSetACHInfo  = Nothing
                        , paymentSetMethodId = pid
                        }
                  finalBody <- case newPayStatus of
                    PaymentFailed _ -> do
                      token  <- tokener
                      reason <- getFailureReason trace payment token
                      tracePrint trace
                                 "getFailureReason "
                                 (usrUserID, evt, payment, reason)
                      case reason of
                        Left e -> do
                          traceError trace
                                     "Error: getFailureReason "
                                     (usrUserID, payId, e)
                          throw $ CantGetFailureReason e
                        Right DwollaFailureReason {..} -> do
                          let payFailure = PaymentFailed dfrCode
                          return $ updateBody { paymentStatus = payFailure }
                    _ -> return updateBody
                  let updatPayment =
                        _UpdatePayment payAuthClientM trace payId finalBody
                  res <- runClientM updatPayment payAuthEnv
                  case res of
                    Left ce -> do
                      traceError trace
                                 "Error: _UpdatePayment "
                                 (usrUserID, payId, ce)
                      throw $ CantUpdatePayment ce
                    Right _ -> return ()
                else do
                  traceError trace
                             "Error: (usrDwollaId /= linkCustomer evt) "
                             (usrUserID, payId, customer)
                  throw $ CantLinkUserToDwollaCustomer customer
