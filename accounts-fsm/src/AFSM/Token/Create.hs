module AFSM.Token.Create where

import           AFSM.DB.Tokens                 ( HasTokenDB(..) )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasEventTracking    ( (.=)
                                                , HasEventTracking(..)
                                                , object
                                                )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( addUTCTime )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Token            ( ExpirationTime(..)
                                                , TokenId
                                                , TokenMedium(..)
                                                , TokenType(..)
                                                , UserToken(..)
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , PhoneNumber(..)
                                                , UserID
                                                , UserModel(usrEmail, usrPhone)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )
import           System.Random                  ( randomRIO )


data TokenInfo = TokenInfo
  { tokenUser  :: UserID
  , tokenTokId :: TokenId
  , tokenType  :: TokenType
  }
  deriving (Eq, Show)

createToken
  :: ( HasTokenDB m
     , HasGetUserDB m
     , GetCurrentTime m
     , HasEventTracking m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> TokenMedium
  -> TokenId
  -> ExpirationTime
  -> m TokenInfo
createToken trace ctcUser ctcMedium ctcTokenId ctcExpiration = do
  tracePrint trace
             "CreateToken "
             (ctcUser, ctcTokenId, ctcMedium, ctcExpiration)

  now  <- getCurrentTime
  user <- fromJust <$> getUserById ctcUser

  let intToText :: [Int] -> Text
      intToText is = T.pack $ concatMap show is

  tokenCode <- liftIO $ intToText <$> mapM (\_ -> randomRIO (1, 9))
                                           [1 :: Integer .. 6]

  let
    token = case ctcMedium of
      EmailMedium -> EmailToken (usrEmail user) tokenCode
      PhoneMedium -> case usrPhone user of
        Nothing ->
          error $ "Error: (CreateToken) No phone number for " <> show ctcUser
        Just phoneNum -> PhoneToken phoneNum tokenCode
      PushMedium -> PushToken tokenCode
  let secondsToAdd = case ctcExpiration of
        TenMinutes -> 60 * 10
        OneHour    -> 60 * 60
        OneDay     -> 60 * 60 * 24
  let userToken = UserToken { tokId        = ctcTokenId
                            , tokRevision  = 1
                            , tokVersion   = "1.0"
                            , tokMsgSource = traceToMID trace
                            , tokToken     = token
                            , tokUser      = ctcUser
                            , tokCreatedOn = now
                            , tokExpiresAt = Just $ addUTCTime secondsToAdd now
                            }

  _ <- saveUserToken userToken

  trackEventToCustomerIO
    ctcUser
    "User Token Created"
    (object
      [ ( "medium"
        , case token of
          EmailToken _ _ -> "Email"
          PhoneToken _ _ -> "SMS"
          PushToken _    -> "Push"
        )
      , "address" .= case token of
        EmailToken (EmailAddress e) _ -> e
        PhoneToken (PhoneNumber  p) _ -> p
        PushToken _                   -> ""
      , "token" .= case token of
        EmailToken _ t -> t
        PhoneToken _ t -> t
        PushToken t    -> t
      ]
    )

  return $ TokenInfo ctcUser ctcTokenId token
