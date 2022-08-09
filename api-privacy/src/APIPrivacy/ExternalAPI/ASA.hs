{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module APIPrivacy.ExternalAPI.ASA
  ( processASA
  ) where

import           APIPrivacy.ExternalAPI.Models as ASA
                                                ( ASAMessage(..)
                                                , ASAResponse(..)
                                                , ASAResult(..)
                                                , AVS(..)
                                                , AVSResult(..)
                                                , MiniCard(..)
                                                )
import           APIPrivacy.Models.Privacy      ( Merchant(..)
                                                , TransactionToken(..)
                                                )
import           APIPrivacy.Monad.HasClient     ( HasClient(..) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Text                     as T
import           Data.UUID.V4                   ( nextRandom )
import           Servant                        ( ServerError
                                                , err404
                                                , err500
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import qualified Shared.Models.Card            as Card
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , TransactionId(TransactionId)
                                                , TransactionSource(..)
                                                )
import           Shared.Models.User             ( UserModel
                                                  ( usrAddressZip
                                                  , usrUserID
                                                  )
                                                )
import           Shared.Utils.Retry             ( retryEither )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(_UsersQueryCard)
                                                , TraceContext
                                                , accountsClientM
                                                , incrementTrace
                                                , randomTrace
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                               as PayAuth
                                                ( AuthResult(..)
                                                , AuthorizePurchaseBody(..)
                                                , Routes(_AuthorizeCardPurchase)
                                                , payAuthClientM
                                                )

processASA
  :: (HasClient m, MonadError ServerError m, MonadIO m)
  => Maybe TraceContext
  -> ASAMessage
  -> m ASAResponse
processASA traceM msg@ASAMessage { token = TransactionToken token, card = MiniCard {..}, merchant = Merchant {..}, avs = AVS {..}, ..}
  = do
    newTid     <- liftIO $ TransactionId <$> nextRandom
    trace      <- maybe randomTrace return traceM
    queryTrace <- incrementTrace trace
    let getUser = _UsersQueryCard accountsClientM queryTrace
          $ Card.PayWithPrivacy cardToken

    userRes <-
      retryEither trace
                  "APIPrivacy.ExternalAPI.ASA.processASA _UsersQueryCard"
                  10
        $ accountsClient getUser
    user <- case userRes of
      Right (u : _) -> return u
      Right []      -> do
        traceError
          trace
          "Error: processASA _UsersQueryCard Could not find a user for this card "
          msg
        throwError err404
      Left e -> do
        traceError
          trace
          "Error: processASA _UsersQueryCard Could not query AccountsEnv "
          (e, msg)
        throwError err500

    let authRequest = AuthorizePurchaseBody
          { transactionId     = newTid
          , source            = PayWithPrivacy
          , sourceId          = token
          , sourceIdempotency = Just token
          , amount            = amount * (-1)
          , description       = descriptor
          , merchant          = Just $ CardMerchant
                                  { cmiMcc = MastercardMCC . T.pack $ show mcc
                                  , cmiMccDesc  = ""
                                  , cmiName     = descriptor
                                  , cmiLocality = Just city
                                  , cmiRegion   = Just state
                                  , cmiCountry  = country
                                  }
          , details           = Nothing
          , cardUsed          = Card.PayWithPrivacy cardToken
          }

    let avsResult = case (zipcode, usrAddressZip user) of
          (Nothing, _      ) -> Nothing
          (_      , Nothing) -> Nothing
          (Just billingZip, Just userZip) ->
            Just $ if billingZip == userZip then Match else Fail

    authTrace <- incrementTrace trace
    let purchaser = usrUserID user
    let authTrx = _AuthorizeCardPurchase payAuthClientM
                                         authTrace
                                         purchaser
                                         authRequest
    authRes <-
      retryEither
          trace
          "APIPrivacy.ExternalAPI.ASA.processASA _AuthorizeCardPurchase"
          10
        $ payAuthClient authTrx
    result <- case authRes of
      Right r -> return r
      Left  e -> do
        traceError trace
                   "Error: processASA - Could not get auth "
                   (purchaser, e, msg)
        throwError err500

    tracePrint trace "Auth result " (result, msg)

    response <- case result of
      Success _ -> return ASAResponse { result    = ASA.Approved
                                      , token     = TransactionToken token
                                      , avsResult = avsResult
                                      , balance   = Nothing
                                      }
      PayAuth.InsufficentFunds _ _ -> return ASAResponse
        { result    = ASA.InsufficentFunds
        , token     = TransactionToken token
        , avsResult = Nothing
        , balance   = Nothing
        }
      _ -> return ASAResponse { result    = ASA.GenericDecline
                              , token     = TransactionToken token
                              , avsResult = Nothing
                              , balance   = Nothing
                              }
    tracePrint trace "ASA response " (usrUserID user, newTid, token, response)
    return response
