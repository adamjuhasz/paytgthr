{-# LANGUAGE RecordWildCards #-}

module APIPrivacy.Example.PrivacyClient where

import           APIPrivacy.Models.Privacy      ( CardState(CardOpen)
                                                , CardType(UnlockedCard)
                                                , Currency(Currency)
                                                , PrivacyCard(..)
                                                )
import           APIPrivacy.PrivacyClient       ( CreateCardBody(..)
                                                , EnrollUserBody(..)
                                                , EnrollUserResponse(..)
                                                , Routes(..)
                                                , SandboxAuthorization(..)
                                                , SandboxAuthorizationResponse(..)
                                                , SandboxVoidClearing(..)
                                                , ShippingAddress(..)
                                                , asClientM
                                                , generatePrivAuth
                                                , mkClientEnv
                                                , newPrivacyManager
                                                , parseBaseUrl
                                                , runClientM
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Time.Clock                ( getCurrentTime )
import           Servant.Client                 ( ClientError(..)
                                                , ResponseF(..)
                                                )

runExample :: IO ()
runExample = do
  manager <- newPrivacyManager
  env     <- mkClientEnv manager <$> parseBaseUrl "https://sandbox.privacy.com"
  now     <- getCurrentTime
  let userInfo = EnrollUserBody { firstName   = "John"
                                , lastName    = "Smith"
                                , dob         = now
                                , street1     = "123 main st"
                                , street2     = Nothing
                                , zipCode     = "12345"
                                , ssnLastFour = "9878"
                                , phoneNumber = "23412312345"
                                , email       = "adam@fake.me"
                                }
  let privacyAuth = generatePrivAuth "00000000-0000-0000-0000-000000000000"

  enrolledUser <- runClientM (_EnrollUser asClientM privacyAuth userInfo) env
  putStrLn "" >> putStr "enrolledUser: " >> print enrolledUser

  accountTok <- case enrolledUser of
    Left (FailureResponse _ Response {..}) -> do
      let body :: ByteString = responseBody
      putStr "Left " >> print (responseStatusCode, body)
      error "non 200 response"
    Left  e                       -> error $ show e
    Right EnrollUserResponse {..} -> return accountToken

  let cardDeats = CreateCardBody
        { cardType        = UnlockedCard
        , cardMemo        = Just "John's card"
        , cardState       = CardOpen
        , spendLimit      = Just $ Currency "USD" 100.30
        , shippingAddress = ShippingAddress { shippingFirstName = ""
                                            , shippingLastName  = ""
                                            , shippingAddress1  = ""
                                            , shippingAddress2  = ""
                                            , shippingCity      = ""
                                            , shippingState     = ""
                                            , shippingZipcode   = ""
                                            }
        }
  card <- runClientM (_CreateCard asClientM privacyAuth accountTok cardDeats)
                     env
  putStrLn "" >> putStr "card: " >> print (accountTok, card)

  (_cardId, Just cardPan) <- case card of
    Left  _                -> error "no card creatd"
    Right PrivacyCard {..} -> return (cardToken, pciPAN)

  accountDeets <- runClientM (_GetAccount asClientM privacyAuth accountTok) env
  putStrLn "" >> putStr "accountDeets " >> print (accountTok, accountDeets)

  cardList <- runClientM (_ListCard asClientM privacyAuth accountTok) env
  putStrLn "" >> putStr "cardList: " >> print cardList

  let trx1Deets = SandboxAuthorization { descriptor = "Targe Bronx 245324"
                                       , pan        = cardPan
                                       , amount     = Currency "USD" 21.43
                                       }
  authRes <- runClientM
    (_SandboxSimAuthorization asClientM privacyAuth trx1Deets)
    env
  putStrLn "" >> putStr "authRes: " >> print authRes
  authToken <- case authRes of
    Left  _ -> error "no auth"
    Right SandboxAuthorizationResponse {..} -> return token

  trxList1 <- runClientM (_ListTransactions asClientM privacyAuth accountTok)
                         env
  putStrLn "" >> putStr "trxList1: " >> print trxList1

  let voidDeets = SandboxVoidClearing { token  = authToken
                                      , amount = Currency "USD" 11.22
                                      }
  voidRes <- runClientM (_SandboxSimVoid asClientM privacyAuth voidDeets) env
  putStrLn "" >> putStr "voidRes: " >> print voidRes

  trxList2 <- runClientM (_ListTransactions asClientM privacyAuth accountTok)
                         env
  putStrLn "" >> putStr "trxList2: " >> print trxList2

  let clearingDeets = SandboxVoidClearing { token = authToken, amount = 34.56 }
  clearRes <- runClientM
    (_SandboxSimClearing asClientM privacyAuth clearingDeets)
    env
  putStrLn "" >> putStr "clearRes: " >> print clearRes

  trxList3 <- runClientM (_ListTransactions asClientM privacyAuth accountTok)
                         env
  putStrLn "" >> putStr "trxList3: " >> print trxList3

  return ()


