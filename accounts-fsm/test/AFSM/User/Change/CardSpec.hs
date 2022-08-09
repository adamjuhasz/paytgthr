{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use &&" -}
{- HLINT ignore "refact:Use &&" -}

module AFSM.User.Change.CardSpec where

import           AFSM.FSM.User                  ( UserEvent(..) )
import           AFSM.User.Change.Card          ( CardUpdate(..)
                                                , changeCardState
                                                )
import           Data.Maybe                     ( isJust )
import           Data.UUID                      ( nil )
import           Shared.Models.Card
import           Shared.Models.User             ( EmailAddress(EmailAddress)
                                                , Password(Password)
                                                , UserChanges(..)
                                                , UserID(UserID)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldSatisfy
                                                )

import           AFSM.IO.Random                 ( HasRandom(..) )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Time.Clock               as Clock
import           Shared.WebAPI.General.API      ( midToTrace )

newtype AppIOM a =
    AppIOM { unAppIOM :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )
instance HasGetUserDB AppIOM where
  getUserById _ = return $ Just basicUser
  getCardsFor _ _ = return
    [ CardModel { cardId       = CardId nil
                , cardPlatform = AptoPayments $ AptoCardId "123"
                , cardRevision = 1
                , cardDesign   = PhysicalBlack
                , cardholder   = UserID nil
                , cardStatus   = CardCreated
                , cardMemo     = Nothing
                , createdAt    = stringToTime "2019-09-02T20:15:32+00:00"
                , activatedAt  = Nothing
                , closedAt     = Nothing
                , updatedAt    = stringToTime "2019-09-02T20:15:32+00:00"
                , cardLastFour = "1234"
                }
    ]
instance HasSaveUserDB AppIOM where
  saveUserModel u
    | and
      [ usrRevision u == 2 -- we bumped rev +1
      , usrAptoCardStatus u == Just CardActive -- recorded state change
      , isJust $ usrCardActivatedOn u -- set date correctly
      ]
    = return ()
    | otherwise
    = error "unkbown user"
  saveCardModel _ card
    | and
      [ cardRevision card == 2
      , cardStatus card == CardActive
      , cardId card == CardId nil
      ]
    = return ()
    | otherwise
    = error $ "bad card model " <> show card
instance HasRandom AppIOM where
  getUUID = return nil
instance GetCurrentTime AppIOM where
  getCurrentTime = liftIO Clock.getCurrentTime

spec :: Spec
spec = do
  describe "Changes the card only" $ do
    it "happy path" $ do
      trace <- midToTrace (MessageID nil)
      let uid = UserID nil
      let fn = changeCardState uid
                               trace
                               (AptoPayments $ AptoCardId "123")
                               (CardAdminChange CardActive)

      res <- unAppIOM fn
      res
        `shouldSatisfy` (\case
                          Right [EventUserInfoChanged _ UserWaitingOnPII [UsersCard], EventUserCardStateChangedFromTo CardCreated CardModel { cardStatus = CardActive }]
                            -> True
                          _ -> False
                        )

basicUser :: UserModel
basicUser = UserModel { usrFirstName          = Just "Basic"
                      , usrLastName           = Just "User"
                      , usrUserState          = UserWaitingOnPII
                      , usrUserID             = UserID nil
                      , usrEmail              = EmailAddress "A@b.c"
                      , usrPassword           = Just $ Password "password"
                      , usrAddressStreet      = Nothing
                      , usrAddressStreet2     = Nothing
                      , usrAddressCity        = Nothing
                      , usrAddressState       = Nothing
                      , usrAddressZip         = Nothing
                      , usrBankRouting        = Nothing
                      , usrBankAcount         = Nothing
                      , usrBankType           = Nothing
                      , usrRevision           = 1
                      , usrVersion            = "1.0"
                      , usrPhone              = Nothing
                      , usrDOB                = Nothing
                      , usrSSN                = Nothing
                      , usrDislcosureOk       = Nothing
                      , usrConstentOk         = Nothing
                      , usrAptoCardholderID   = Nothing
                      , usrAptoKYCStatus      = Nothing
                      , usrAptoCardId         = Nothing
                      , usrAptoCardStatus     = Nothing
                      , usrMsgSource          = MessageID nil
                      , usrBankAccountName    = Nothing
                      , usrBankName           = Nothing
                      , usrDwollaId           = Nothing
                      , usrDwollaFundingId    = Nothing
                      , usrBankVerifedAmounts = Nothing
                      , usrBankVerified       = Nothing
                      , usrCreatedOn = stringToTime "2019-09-02T20:15:32+00:00"
                      , usrFirstSignIn        = Nothing
                      , usrBecameActiveOn     = Nothing
                      , usrCardCreatedOn      = Nothing
                      , usrCardActivatedOn    = Nothing
                      , usrPrivacyAcctToken   = Nothing
                      , usrEmailVerified      = False
                      , usrPhoneVerified      = False
                      }
