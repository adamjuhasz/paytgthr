module PaymentAuth.Monad.Plaid where

import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Servant.Client                 ( ClientEnv )
import           Shared.Models.Plaid.Base       ( AccessToken
                                                , Account
                                                , AuthResponse
                                                , ItemId
                                                , PlaidAccountId
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.TgthrMessages.Base      ( MessageID )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PlaidEnvironment )

type MeasuredTime = Double

type ABANumber = Text
type DDANumber = Text
class Monad m => HasPlaidDB m where
  getPlaidEnv           :: m ClientEnv
  getPlaidClientId      :: m Text
  getPlaidSecret        :: m Text
  insertToken           :: MessageID -> UserID -> (AccessToken, ItemId, PlaidEnvironment) -> m ()
  getAccessToken        :: UserID -> m (Maybe (AccessToken, PlaidEnvironment))
  getPlaidAuth          :: (AccessToken, PlaidEnvironment) -> m (Either Text AuthResponse)
  saveBalance           :: MessageID -> UserID -> (Account, MeasuredTime) -> m ()
  getPrimaryAccount     :: UserID -> m (Maybe PlaidAccountId)
  updateTokenPrimary    :: MessageID -> UserID -> (PlaidAccountId, ABANumber, DDANumber) -> m ()
  getUsersBalanceWithin :: UserID -> UTCTime -> m (Maybe (Rational, UTCTime))
