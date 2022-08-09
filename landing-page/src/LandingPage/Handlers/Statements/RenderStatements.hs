{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

module LandingPage.Handlers.Statements.RenderStatements where

import           Data.Time                      ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.LocalTime            ( utcToZonedTime )
import           Data.UUID                      ( fromText )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( getSessionUser )
import           Network.HTTP.Types.Status      ( status404
                                                , status500
                                                )
import           Shared.Amqp                    ( AMQPPublisher )
import           Shared.Amqp.Utils              ( getLastTransactions )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                )
import           Shared.Models.Transaction      ( MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Printf                    ( printf )
import           Text.RawString.QQ              ( r )
import           Web.Scotty                     ( ActionM
                                                , finish
                                                , html
                                                , liftAndCatchIO
                                                , param
                                                , status
                                                , text
                                                )

getTransactions
  :: (Transaction -> Bool) -> UserID -> AMQPPublisher -> ActionM ()
getTransactions f user pub = do
  trxsE <- liftAndCatchIO $ getLastTransactions pub user
  case trxsE of
    Left e -> do
      liftAndCatchIO
        $  putStr "Error: could not get trxs for statement "
        >> print (user, e)
      status status500 >> text "Error"
    Right trxs -> html . renderHtml . renderStatement user $ filter f trxs

get12MonthStatement :: V.Key SessionData -> AMQPPublisher -> ActionM ()
get12MonthStatement sKey pub = do
  user <- getSessionUser sKey
  getTransactions (const True) user pub

get24MonthStatement :: V.Key SessionData -> AMQPPublisher -> ActionM ()
get24MonthStatement sKey pub = do
  user <- getSessionUser sKey
  getTransactions (const True) user pub

getAnyUserStatement :: AMQPPublisher -> ActionM ()
getAnyUserStatement pub = do
  maybeUUID <- fromText <$> param "id"
  user      <- case maybeUUID of
    Nothing   -> status status404 >> finish
    Just uuid -> return $ UserID uuid
  getTransactions (const True) user pub

showDateTime :: UTCTime -> Html
showDateTime time =
  toHtml $ formatTime defaultTimeLocale "%m-%d-%Y" $ utcToZonedTime timeZone
                                                                    time
  where timeZone = read "PDT"

showCurr :: Currency -> Html
showCurr (Currency "USD" val) =
  let f :: Double         = fromRational val
      formatted :: String = printf "$%.2f" f
  in  toHtml formatted
showCurr (Currency iso val) =
  let f :: Double         = fromRational val
      formatted :: String = printf "%s %.2f" iso f
  in  toHtml formatted

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderStatement :: UserID -> [Transaction] -> Html
renderStatement thisUser allTrxs = do
  let trxsToShow = filter (\Transaction {..} -> getMonetaryValue trxDisplayAmount /= 0) allTrxs

  H.html $ do
    H.head $ do
      H.title "Pay Tgthr Statement"
      H.meta ! A.charset "utf-8"
      H.link ! A.rel "stylesheet" ! A.href "https://unpkg.com/tailwindcss@^1.5.1/dist/tailwind.min.css"
      H.link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/npm/@tailwindcss/ui@0.6.0/dist/tailwind-ui.css"
      H.link ! A.rel "stylesheet" ! A.href "https://rsms.me/inter/inter.css"
      H.style $ do
        [r| 
          @media  print {
            thead { 
              display: table-row-group;
            }
          }    

          tr {
            break-inside: avoid;
          } 
        |]
    H.body $ do
      H.div ! A.class_ "flex flex-col" $ do
        H.div ! A.class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" $ do
          H.h3 ! A.class_ "text-lg leading-6 font-medium text-gray-900 mb-2" $ do
            "Pay Tgthr debit Mastercard Statement"
          H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
            H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
              H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
                H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
                  H.thead $ do
                    let headerClasses = "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider"
                    H.tr $ do
                      H.th ! A.class_ headerClasses $ do
                        "Date"
                      H.th ! A.class_ headerClasses $ do
                        "Store"
                      H.th ! A.class_ headerClasses $ do
                        "Amount"
                      H.th ! A.class_ headerClasses $ do
                        "Purchased by"
                      H.th ! A.class_ headerClasses $ do
                        "Status"
                  H.tbody $ do
                    let firstCellClasses = "px-6 py-4 whitespace-no-wrap text-sm leading-5 font-medium text-gray-900"
                    let cellClasses      = "px-6 py-4 whitespace-no-wrap text-sm leading-5 text-gray-500"
                    let renderTrxRow Transaction {..} = do
                          H.tr ! A.class_ "bg-white" $ do
                            H.td ! A.class_ firstCellClasses $ do
                              showDateTime trxPurchasedAt
                            H.td ! A.class_ cellClasses $ do
                              case (trxMerchant, trxDetails) of
                                (Just CardMerchant {..}, _) -> toHtml cmiName
                                (_, Just CardTransaction { pcpDescription = Just t }) -> toHtml t
                                (_                     , _) -> "Unknown"
                            H.td ! A.class_ cellClasses $ do
                              showCurr trxDisplayAmount
                            H.td ! A.class_ cellClasses $ do
                              if trxUserId == thisUser then "You" else "Partner"
                            H.td ! A.class_ cellClasses $ do
                              case trxState of
                                TrxCreated         -> "Pending"
                                TrxAuthorized      -> "Pending"
                                TrxPending         -> "Pending"
                                TrxPendingReversal -> "Reversal"
                                TrxDeclined _      -> "Declined"
                                TrxCompleted       -> "Completed"
                                TrxDisputed _      -> "Disputed"
                    mapM_ renderTrxRow trxsToShow
