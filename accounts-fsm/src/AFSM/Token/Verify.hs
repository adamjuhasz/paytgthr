{-# LANGUAGE RecordWildCards #-}

module AFSM.Token.Verify where

import           AFSM.DB.Tokens                 ( HasTokenDB(..) )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           Control.Monad.Reader           ( MonadIO(..) )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Token            ( TokenMedium(..)
                                                , TokenType(..)
                                                , UserToken(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data VerificationState
  = UserTokenVerified
  | UserTokenNotFound
  | UserTokenExpired
  deriving (Eq, Show)

tokTypeToMediun :: TokenType -> TokenMedium
tokTypeToMediun EmailToken{} = EmailMedium
tokTypeToMediun PhoneToken{} = PhoneMedium
tokTypeToMediun PushToken{}  = PushMedium

normalizeToken :: Text -> Text
normalizeToken = T.strip

verifyToken
  :: (HasTokenDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> TokenMedium
  -> Text
  -> m VerificationState
verifyToken trace vtcUser vtcMedium vtcTokenCode = do
  let normalizedCode = normalizeToken vtcTokenCode

  tracePrint trace
             "VerifyToken "
             (vtcUser, vtcMedium, vtcTokenCode, normalizedCode)

  tokens <- getUserTokenWithCode normalizedCode vtcUser
  now    <- getCurrentTime

  let activeTokens =
        filter (\UserToken {..} -> tokTypeToMediun tokToken == vtcMedium)
          . filter (\UserToken {..} -> all (> now) tokExpiresAt)
          $ tokens

  let matchResult = case (null tokens, null activeTokens) of
        (True , _    ) -> UserTokenNotFound
        (_    , True ) -> UserTokenExpired
        (False, False) -> UserTokenVerified

  tracePrint trace
             "VerifyToken completed "
             (vtcUser, vtcMedium, vtcTokenCode, matchResult, tokens)

  return matchResult
