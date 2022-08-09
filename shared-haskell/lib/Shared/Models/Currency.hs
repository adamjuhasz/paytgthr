module Shared.Models.Currency where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , withArray
                                                )
import           Data.Ratio                     ( (%)
                                                , denominator
                                                , numerator
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Vector                    ( (!) )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           Text.Printf                    ( printf )

type ISOCode = Text
type MValue = Rational
data Currency = Currency ISOCode MValue
  deriving (Show, Read)

simplifiedRational :: Double -> Rational
simplifiedRational d = toRational (d * 100) / 100

currencyToDouble :: Currency -> Double
currencyToDouble (Currency _ v) = fromRational v

showCurr :: Currency -> String
showCurr (Currency _ val) =
  let f :: Double = fromRational val
  in  if fromIntegral (floor f :: Int) == f
        then printf "$%.0f" f
        else printf "$%.2f" f

instance FromJSON Currency where
  parseJSON = withArray "Currency" $ \a -> do
    c <- parseJSON $ a ! 0
    n <- parseJSON $ a ! 1
    d <- parseJSON $ a ! 2

    let toDouble :: Rational -> Double
        toDouble = fromRational
    let rationalNum         = n % d
    let doubleNum :: Double = toDouble rationalNum
    let simpleRational      = floor (doubleNum * 100) % 100

    -- We try to simply complex rationals so our storage engines can handle it
    return . Currency c $ if toDouble simpleRational == toDouble rationalNum
      then simpleRational
      else rationalNum

instance ToJSON Currency where
  toJSON (Currency c v) =
    let numer               = numerator v
        denom               = denominator v
        doubleNum :: Double = fromRational v
        simpleNumer         = floor (doubleNum * 100)
        simpleDeno          = 100
        simpleRatio         = simpleNumer % simpleDeno
    -- We try to simply complex rationals so our storage engines can handle it
    in  toJSON $ if simpleRatio == v
          then (c, simpleNumer, simpleDeno)
          else (c, numer, denom)

instance ToField Currency where
  toField = toJSONField

instance FromField Currency where
  fromField = fromJSONField

getIsoCode :: Currency -> Text
getIsoCode (Currency t _) = t

getMonetaryValue :: Currency -> MValue
getMonetaryValue (Currency _ v) = v

instance Num Currency where
  (+) (Currency code1 val1) (Currency code2 val2)
    | code1 == code2 = Currency code1 ((+) val1 val2)
    | code2 == ""    = Currency code1 ((+) val1 val2)
    | code1 == ""    = Currency code2 ((+) val1 val2)
    | otherwise      = error "Different ISO Codes"
  (-) (Currency code1 val1) (Currency code2 val2)
    | code1 == code2 = Currency code1 ((-) val1 val2)
    | code2 == ""    = Currency code1 ((-) val1 val2)
    | code1 == ""    = Currency code2 ((-) val1 val2)
    | otherwise      = error "Different ISO Codes"
  (*) (Currency code1 val1) (Currency code2 val2)
    | code1 == code2 = Currency code1 ((*) val1 val2)
    | code2 == ""    = Currency code1 ((*) val1 val2)
    | code1 == ""    = Currency code2 ((*) val1 val2)
    | otherwise      = error "Different ISO Codes"
  negate (Currency code val) = Currency code (negate val)
  signum (Currency code val) = Currency code (signum val)
  abs (Currency code val) = Currency code (abs val)
  fromInteger int = Currency "" (toRational int)

instance Fractional Currency where
  (/) (Currency code1 val1) (Currency code2 val2)
    | code1 == code2 = Currency code1 ((/) val1 val2)
    | code2 == ""    = Currency code1 ((/) val1 val2)
    | otherwise      = error "Different ISO Codes"
  fromRational x = Currency "" (fromRational x)

instance Eq Currency where
  (==) (Currency code1 val1) (Currency code2 val2)
    | code1 == code2 = val1 == val2
    | code2 == ""    = val1 == val2
    | code1 == ""    = val1 == val2
    | otherwise      = False

instance Ord Currency where
  compare (Currency code1 val1) (Currency code2 val2)
    | code1 == code2 = compare val1 val2
    | code2 == "" = compare val1 val2
    | code1 == "" = compare val1 val2
    | otherwise = error
      ("Different ISO Codes of " <> T.unpack code1 <> " and " <> T.unpack code2)

roundUpUSD :: Currency -> Currency
roundUpUSD = roundUp 2

roundDownUSD :: Currency -> Currency
roundDownUSD = roundDown 2

roundUp :: Int -> Currency -> Currency
roundUp places (Currency iso val) =
  let twodec = ceiling (val * (10 ^ places))
      ratty  = twodec % (10 ^ places)
  in  Currency iso ratty

roundDown :: Int -> Currency -> Currency
roundDown places (Currency iso val) =
  let twodec = floor (val * (10 ^ places))
      ratty  = twodec % (10 ^ places)
  in  Currency iso ratty
