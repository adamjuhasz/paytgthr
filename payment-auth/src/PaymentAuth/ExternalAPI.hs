{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.ExternalAPI where

import           Control.Concurrent             ( MVar
                                                , tryReadMVar
                                                )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Vault.Lazy               as V
import           Network.HTTP.Types.Status      ( status400
                                                , status503
                                                )
import           Network.Wai                    ( Application
                                                , Request(vault)
                                                )
import           Network.Wai.Middleware.AddHeaders
                                                ( addHeaders )
import           PaymentAuth.Types              ( DBActions(..) )
import           Shared.Messages                ( CommandMessage(PayCmd)
                                                , MessageBody(CommandV1)
                                                , TgthrMessage
                                                )
import           Shared.Models.Plaid.Webhook    ( TransactionWebhooks(..)
                                                , WebhookType(Transactions)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PaymentCmd(..) )
import           Web.Scotty                     ( finish
                                                , get
                                                , jsonData
                                                , liftAndCatchIO
                                                , middleware
                                                , post
                                                , request
                                                , rescue
                                                , scottyApp
                                                , status
                                                , text
                                                )

data Environment
  = Production
  | Development
  | Testing

data ApplicationConfig = ApplicationConfig
  { sendMessage    :: Maybe TgthrMessage -> MessageBody -> IO TgthrMessage
  , environment    :: Environment
  , db             :: DBActions
  , isShuttingDown :: MVar Bool
  }

app :: ApplicationConfig -> IO Application
app config = do
  msgKey <- V.newKey
  scottyApp $ do
    middleware $ addHeaders [("Cache-Control", "no-cache")]

    get "/ping" $ do
      res <- liftAndCatchIO $ tryReadMVar (isShuttingDown config)
      case res of
        Nothing    -> text "pong"
        Just False -> text "pong"
        Just True  -> do
          liftAndCatchIO $ putStrLn "SIGTERM received... returning 503 External"
          status status503 >> text "Not Ok"

    post "/webhook" $ do
      incoming :: WebhookType <- jsonData
        `rescue` const (status status400 >> text "Malformed" >> finish)
      origMSG <- fromJust . V.lookup msgKey . vault <$> request
      debug   <- case incoming of
        Transactions x ->
          liftAndCatchIO $ publishBalanceRefresh config origMSG x
        _ -> return "Unknown"
      text debug

publishBalanceRefresh
  :: ApplicationConfig -> TgthrMessage -> TransactionWebhooks -> IO Text
publishBalanceRefresh (ApplicationConfig publish _ DBActions {..} _) origMSG DefaultUpdate {..}
  = do
    maybeUser <- dbGetUserFromItem duItemID
    let user = fromMaybe (error "error: unknwon item for user") maybeUser
    let cmd  = CommandV1 . PayCmd . GetSpendableBalance $ user
    _ <- publish (Just origMSG) cmd
    return "Transactions"
publishBalanceRefresh _ _ _ = return "Unkown"
