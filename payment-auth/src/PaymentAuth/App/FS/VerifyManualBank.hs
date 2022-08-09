module PaymentAuth.App.FS.VerifyManualBank where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.List                      ( sort )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           Shared.Console                 ( traceError )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data VerifyManaulBankErrors
  = CantFindUser UserID
  | NoVerificationPayments UserID
  deriving (Show)
instance Exception VerifyManaulBankErrors

verifyManualBank
  :: (HasAccounts m, MonadIO m) => TraceContext -> UserID -> [Double] -> m Bool
verifyManualBank trace vmcUser vmcAmounts = do
  userM <- getUser trace vmcUser
  user  <- case userM of
    Just u  -> return u
    Nothing -> do
      traceError trace "Error: Can't get user" vmcUser
      throw $ CantFindUser vmcUser

  recordedAmounts <- case usrBankVerifedAmounts user of
    Nothing -> do
      traceError trace
                 "Error: User missing usrBankVerifedAmounts"
                 (vmcUser, vmcAmounts)
      throw $ NoVerificationPayments vmcUser
    Just amounts -> return amounts

  let amountsMatch = sort recordedAmounts == sort vmcAmounts

  return amountsMatch
