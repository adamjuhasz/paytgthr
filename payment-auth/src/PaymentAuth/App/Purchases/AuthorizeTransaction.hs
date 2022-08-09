{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use &&" -}

{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module PaymentAuth.App.Purchases.AuthorizeTransaction
  ( authorizeNewTransaction
  , isGroupOk
  , findSplitForTransaction
  , matchCategories
  , IsGroupOk(..)
  ) where

import           Control.Monad                  ( forM )
import           Control.Monad.Catch            ( MonadCatch(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Either                    ( lefts )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(Down) )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( nil )
import           PaymentAuth.App.AuthorizeTransaction
                                                ( authorizeTransaction )
import           PaymentAuth.App.RiskManagement.UpdateWorkflow
                                                ( ExtraFacts(..)
                                                , riskWorkFlow
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Card            as Card
                                                ( CardId(CardId)
                                                , CardModel(..)
                                                , CardStatus(..)
                                                , IssuerPlatform(..)
                                                )
import           Shared.Models.CategorySplit    ( CategoryCode(..)
                                                , CategorySplit(..)
                                                , CategoryState(..)
                                                )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupSplit(..)
                                                , groupIsActive
                                                )
import           Shared.Models.Ids              ( GroupId
                                                , UserID
                                                )
import           Shared.Models.RiskScore        ( RiskFact(..) )
import           Shared.Models.Transaction     as Trx
                                                ( DeclineReason(..)
                                                , MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionEvent(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                               as PA
                                                ( AuthResult(..)
                                                , RiskRules(..)
                                                , extractTrxFromAuthResult
                                                )
import           Shared.Transactions.Categorize ( categorize )
import           Shared.WebAPI.General.API      ( TraceContext )

findUserError
  :: Transaction
  -> (Maybe UserModel, UserID)
  -> Either (UserID, AuthResult) UserID
findUserError trxWithGroup (Nothing, uid) = Left . (uid, ) $ UserMissing
  (trxWithGroup { trxState = TrxDeclined (UserNotFound [uid]) })
  [uid]
findUserError trxWithGroup (Just UserModel { usrBankVerified = Nothing }, uid)
  = Left . (uid, ) $ NoPaymentLinked
    (trxWithGroup { trxState = TrxDeclined (PaymentUnlinked [uid]) })
    [uid]
findUserError trxWithGroup (Just UserModel { usrBankVerified = Just False }, uid)
  = Left . (uid, ) $ NoPaymentLinked
    (trxWithGroup { trxState = TrxDeclined (PaymentUnlinked [uid]) })
    [uid]
findUserError _ (Just UserModel { usrUserState = UserActive }, uid) = Right uid
findUserError trxWithGroup (Just UserModel { usrUserState = UserCreated }, uid)
  = Left . (uid, ) $ AccountNotActive
    (trxWithGroup { trxState = TrxDeclined (UserNotActive [uid]) })
    [uid]
findUserError trxWithGroup (Just UserModel { usrUserState = UserWaitingOnPII }, uid)
  = Left . (uid, ) $ AccountNotActive
    (trxWithGroup { trxState = TrxDeclined (UserNotActive [uid]) })
    [uid]
findUserError trxWithGroup (Just UserModel { usrUserState = UserWaitingOnKYC }, uid)
  = Left . (uid, ) $ AccountNotActive
    (trxWithGroup { trxState = TrxDeclined (UserNotActive [uid]) })
    [uid]
findUserError trxWithGroup (Just UserModel { usrUserState = UserKYCDelay }, uid)
  = Left . (uid, ) $ AccountNotActive
    (trxWithGroup { trxState = TrxDeclined (UserNotActive [uid]) })
    [uid]
findUserError trxWithGroup (Just UserModel { usrUserState = UserUpdated }, uid)
  = Left . (uid, ) $ AccountNotActive
    (trxWithGroup { trxState = TrxDeclined (UserNotActive [uid]) })
    [uid]
findUserError trxWithGroup (Just UserModel { usrUserState = UserUpdatedKYCDelay }, uid)
  = Left . (uid, ) $ AccountNotActive
    (trxWithGroup { trxState = TrxDeclined (UserNotActive [uid]) })
    [uid]
findUserError trxWithGroup (Just UserModel { usrUserState = UserClosed _ }, uid)
  = Left . (uid, ) $ AccountClosed
    (trxWithGroup { trxState = TrxDeclined Trx.CardClosed })
    [uid]

data IsGroupOk
  = GroupNotOk
  | GroupOK GroupModel

isGroupOk :: UTCTime -> Maybe GroupModel -> IsGroupOk
isGroupOk _ Nothing = GroupNotOk
isGroupOk now (Just g) | groupIsActive now g = GroupOK g
                       | otherwise           = GroupNotOk

type PreviousTransaction = Transaction
authorizeNewTransaction
  :: ( HasRiskScoresDB m
     , HasTime m
     , HasAccounts m
     , HasTransactionsDB m
     , HasLedgerDB m
     , HasPaymentsDB m
     , MonadIO m
     , HasEventTracking m
     , MonadCatch m
     )
  => TraceContext
  -> IssuerPlatform
  -> Transaction
  -> m (AuthResult, Maybe PreviousTransaction)
authorizeNewTransaction trace card trxCreated = do
  -- who swiped their card
  let purchaser = trxUserId trxCreated

  -- is there a previoius transaction we need to update?
  prevTrx <- getTransactionUsingSourceId trace $ trxSourceId trxCreated

  (groupInfo, splitAmounts) <- findSplitForTransaction trace
                                                       trxCreated
                                                       purchaser
  let trxWithGroup =
        trxCreated { trxGroupId = groupInfo, trxSplitAmounts = splitAmounts }
  let userList = fst <$> splitAmounts

  -- grab involved users state's
  usersM    <- forM userList (getUser trace)
  cardMaybe <- findCard trace card
  let cState = maybe CardActive cardStatus cardMaybe
  let cId    = maybe (CardId nil) cardId cardMaybe

  -- verify out if users are "active" and have funding sources linked
  let badUsers =
        lefts $ fmap (findUserError trxWithGroup) (zip usersM userList)

  -- auth transaction as needed
  result <-
    case (isDigitalWalletAction trxWithGroup, groupInfo, badUsers, cState) of
    -- always allow digital wallet stuff
      (True, _, _, _) -> return $ Success trxWithGroup
        { trxState        = TrxAuthorized
        , trxDescription  = Just "Digital Wallet Action"
        , trxSourceEvent  = NonFinancialEvent
        , trxGroupId      = Nothing
        , trxSplitAmounts = [(purchaser, 100)]
        }
      -- No solo transactions allowed for bad groups
      (False, Nothing, _, _) -> return $ GroupNotActive $ trxWithGroup
        { trxState = TrxDeclined GroupError
        }
      -- All users have correct state
      (False, Just _, [], CardActive) ->
        authorizeTransaction trace prevTrx trxWithGroup
      -- Partner of purchaser is not able to make a purchase so we decline
      (False, Just _, (_uid, authResult) : _, _) -> return authResult -- should be smarter by concating
      (_, _, _, CardCreated) -> return $ PA.CardNotActivated
        (trxWithGroup { trxState = TrxDeclined Trx.CardNotActivated })
        cId
      (_, _, _, CardUserFrozen) -> return $ PA.CardNotActivated
        (trxWithGroup { trxState = TrxDeclined Trx.CardInactive })
        cId
      (_, _, _, CardAdminFrozen) -> return $ PA.CardNotActivated
        (trxWithGroup { trxState = TrxDeclined Trx.CardInactive })
        cId
      (_, _, _, Card.CardClosed) -> return $ PA.CardNotActivated
        (trxWithGroup { trxState = TrxDeclined Trx.CardClosed })
        cId

  let finaltrx     = extractTrxFromAuthResult result
  let tid          = trxId finaltrx

  -- adjust riskscore of purchaser if needed
  let riskAdjuster = riskWorkFlow trace purchaser
  _ <- case result of
    RiskTriggered _ RiskyMerchantName ->
      riskAdjuster (RiskyTransaction tid) NoExtraInfo
    RiskTriggered _ RiskyMerchantMCC ->
      riskAdjuster (RiskyTransaction tid) NoExtraInfo
    RiskTriggered _ RiskyP2P -> riskAdjuster (RiskyTransaction tid) NoExtraInfo
    RiskTriggered _ CombinedRiskRules ->
      riskAdjuster (RiskyTransaction tid) NoExtraInfo
    _ -> return Nothing

  -- save transaction
  saveTransaction trace finaltrx

  -- debug
  tracePrint trace
             "(AuthorizeTransaction) result: "
             (trxUserId trxCreated, result)

  -- return AuthResult to asker
  return (result, prevTrx)

isDigitalWalletAction :: Transaction -> Bool
isDigitalWalletAction Transaction {..} =
  let correctMCC = case cmiMcc <$> trxMerchant of
        Nothing                     -> False
        Just (MastercardMCC "5969") -> True
        Just (MastercardMCC _     ) -> False
      correctMerchant = case cmiName <$> trxMerchant of
        Nothing           -> False
        Just "Mastercard" -> True
        Just _            -> False
      correctAmount = case trxDisplayAmount of
        Currency "USD" 0 -> True
        Currency _     _ -> False
  in  and [correctMCC, correctMerchant, correctAmount]

findSplitForTransaction
  :: (HasAccounts m, HasTime m)
  => TraceContext
  -> Transaction
  -> UserID
  -> m (Maybe (GroupId, Int), [(UserID, Rational)])
findSplitForTransaction trace trx purchaser = do
  now         <- getCurrentTime
  groupEither <- getGroupFor trace purchaser

  let soloPurchase = (Nothing, [(purchaser, 100)])

  case isGroupOk now groupEither of
    GroupNotOk -> return soloPurchase
    GroupOK GroupModel { grpId = gid, grpRevision = rev } -> do
      categorySplits <- getCategorySplits trace gid

      let applicableCats = matchCategories categorySplits $ categorize trx

      case applicableCats of
        [] -> return soloPurchase
        CategorySplit {..} : _ ->
          return
            ( Just (gid, rev)
            , fmap (\GroupSplit {..} -> (splUser, splRatio)) splits
            )

matchCategories :: [CategorySplit] -> [(CategoryCode, Bool)] -> [CategorySplit]
matchCategories categorySplits matchingCategories =
  let enabledCatSplits = sortOn (Down . categoryId) $ filter
        (\CategorySplit { state = catState } -> catState == CategoryActive)
        categorySplits
      applicableCats = filter
        (\CategorySplit {..} -> case lookup categoryId matchingCategories of
          Nothing    -> False
          Just False -> False
          Just True  -> True
        )
        enabledCatSplits
  in  applicableCats
