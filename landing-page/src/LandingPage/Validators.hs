{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{- HLINT ignore "Use head" -}


module LandingPage.Validators where

import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime
                                                , formatTime
                                                , defaultTimeLocale
                                                , nominalDay
                                                , diffUTCTime
                                                , parseTimeM
                                                )
import           Data.Functor                   ( (<&>) )
import           GHC.Stack                      ( HasCallStack )
import           LandingPage.Validators.TLDs    ( validTLDs )
import           Shared.Models.User             ( Password(..) )
import           Text.RawString.QQ              ( r )
import           Text.Regex.TDFA                ( (=~) )
import           Text.Regex.TDFA.Text           ( )

stack :: HasCallStack => [a -> Maybe b] -> (a -> Maybe b)
stack [] _ = error "can't be empty list"
stack fs x =
  let h = head fs
      t = tail fs
  in  foldr (\f accum -> if isJust accum then f x else accum) (h x) t

-- | Normalize a date
dateCheck :: Text -> Maybe Text
dateCheck date = case (date =~ dateRegex, date =~ dateRegexFlipped) of
  (True, _) ->
    parser (head compsR <> "/" <> compsR !! 1 <> "/" <> compsR !! 2)
      <&> formatter
  (_, True) ->
    parser (compsF !! 1 <> "/" <> compsF !! 2 <> "/" <> head compsF)
      <&> formatter
  _ -> Nothing
 where
  parser :: Text -> Maybe UTCTime
  parser = parseTimeM True defaultTimeLocale "%-m/%-d/%Y" . T.unpack
  formatter :: UTCTime -> Text
  formatter         = T.pack . formatTime defaultTimeLocale "%m/%d/%0Y"
  (_, _, _, compsR) = date =~ dateRegex :: (Text, Text, Text, [Text])
  (_, _, _, compsF) = date =~ dateRegexFlipped :: (Text, Text, Text, [Text])

dateRegex :: String
dateRegex = [r|\`([0-9]{1,2})[-\\\/]?([0-9]{1,2})[-\\\/]?([0-9]{4})\'|]
dateRegexFlipped :: String
dateRegexFlipped = [r|\`([0-9]{4})[-\\\/]?([0-9]{1,2})[-\\\/]?([0-9]{1,2})\'|]

ageVerification :: UTCTime -> Text -> Maybe Text
ageVerification now dob = case normalized >>= usersAge of
  Nothing -> Nothing
  Just d  -> if d >= eighteen && d < oneThirty then normalized else Nothing
 where
  normalized = dateCheck dob
  usersAge d = parser d <&> diffUTCTime now
  eighteen  = nominalDay * 365 * 18 -- Min age is 18
  oneThirty = nominalDay * 365 * 130 -- Max age is 130
  parser :: Text -> Maybe UTCTime
  parser = parseTimeM True defaultTimeLocale "%-m/%-d/%Y" . T.unpack

-- | Normalize a phone Number
phoneKeeper :: Char -> Bool
phoneKeeper '0' = True
phoneKeeper '1' = True
phoneKeeper '2' = True
phoneKeeper '3' = True
phoneKeeper '4' = True
phoneKeeper '5' = True
phoneKeeper '6' = True
phoneKeeper '7' = True
phoneKeeper '8' = True
phoneKeeper '9' = True
phoneKeeper _   = False

stripLeading :: Text -> Text
stripLeading t = if T.length t > 1 && (T.head t == '0' || T.head t == '1')
  then stripLeading $ T.tail t
  else t

phoneCheck :: Text -> Maybe Text
phoneCheck number = if str /= "" || length comps /= 3
  then Nothing
  else Just $ area <> prefix <> final
 where
  normalizedNumber = stripLeading . T.filter phoneKeeper $ number
  (str, _, _, comps) =
    normalizedNumber =~ phoneRegex :: (Text, Text, Text, [Text])
  area   = comps !! 0
  prefix = comps !! 1
  final  = comps !! 2

phoneRegex :: String
phoneRegex = [r|\`([0-9]{3})([0-9]{3})([0-9]{4})\'|]

-- | Normalize an email address
emailCheck :: Text -> Maybe Text
emailCheck email = if isEmail && isNotExample && isNotTgthr && isValidDomain
  then Just email
  else Nothing
 where
  isEmail      = email =~ emailRegex :: Bool
  isNotExample = not $ email =~ exampleDomainRegex :: Bool
  isNotTgthr   = not $ email =~ tgthrDomainRegex :: Bool
  (_, _, _, domain) =
    T.toUpper email =~ domainExtractRegex :: (Text, Text, Text, [Text])
  isValidDomain = head domain `elem` validTLDs

emailRegex :: String
emailRegex =
  [r|\`[a-zA-Z0-9.!#$%&’*+\/=?^_`{|}~-]+@[a-zA-Z0-9.-]+\.[a-zA-Z0-9-]+\'|]

exampleDomainRegex :: String
exampleDomainRegex = [r|@example\.[a-zA-Z0-9-]+\'|]

tgthrDomainRegex :: String
tgthrDomainRegex = [r|@paytgthr\.com\'|]

domainExtractRegex :: String
domainExtractRegex = [r|\.([a-zA-Z0-9-]+)\'|]

-- | Normalize a SSN
-- Rules: https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs
ssnCheck :: Text -> Maybe Text
ssnCheck ssn =
  if (str /= "")
       || (length comps /= 3)
       || startsWith666
       || startsWith000
       || groupIs000
       || serialIs0000
    then Nothing
    else Just normalizedSSN
 where
  (str, _, _, comps) = ssn =~ ssnRegex :: (Text, Text, Text, [Text])
  area               = head comps
  group              = comps !! 1
  serial             = comps !! 2
  normalizedSSN      = area <> group <> serial
  isTestingSSN       = normalizedSSN == "123120000"
  startsWith666      = area == "666"
  startsWith000      = area == "000"
  groupIs000         = group == "00"
  serialIs0000       = serial == "0000" && not isTestingSSN -- Testing SSN

ssnRegex :: String
ssnRegex = [r|\`([0-9]{3})[- ]?([0-9]{2})[- ]?([0-9]{4})\'|]

-- | Verify password rules
-- Rules: https://en.wikipedia.org/wiki/Password_policy#NIST_guidelines
passwordCheck :: Password -> Maybe Password
passwordCheck p@(Password pass) =
  if T.length pass >= 8 then Just p else Nothing

isFixedLengthNumber :: Int -> Text -> Maybe Text
isFixedLengthNumber len t = if T.length t == len then Just t else Nothing

isBothEqual :: Text -> Text -> Maybe Text
isBothEqual t t' = if t == t' then Just t else Nothing

isNotEmpty :: Text -> Maybe Text
isNotEmpty "" = Nothing
isNotEmpty t  = Just t

stateCheck :: Text -> Maybe Text
stateCheck state = if is2Letter then Just state else Nothing
  where is2Letter = state =~ stateRegex :: Bool

stateRegex :: String
stateRegex =
  [r|\`(AL|AK|AZ|AR|CA|CO|CT|DE|DC|FL|GA|HI|ID|IL|IN|IA|KS|KY|LA|ME|MD|MA|MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|MP|OH|OK|OR|PA|RI|SC|SD|TN|TX|UT|VT|VA|WA|WV|WI|WY)\'|]

zipCheck :: Text -> Maybe Text
zipCheck zipcode = if str /= "" || null comps
  then Nothing
  else Just $ head comps
  where (str, _, _, comps) = zipcode =~ zipRegex :: (Text, Text, Text, [Text])

zipRegex :: String
zipRegex = [r|\`([0-9]{5})[-]?([0-9]{4})?\'|]

poboxCheck :: Text -> Maybe Text
poboxCheck address = if not isPoBox then Just address else Nothing
 where
  normalized = T.strip . T.toLower . T.replace "." "" $ address
  isPoBox    = normalized =~ poboxRegex :: Bool

poboxRegex :: String
poboxRegex =
  [r|\`.*(box|pob|po|po box|pobox)[ ]*(num|#|number|no)*[ ]*[0-9]+.*\'|]

isInteger :: Text -> Maybe Text
isInteger t = if itis then Just t else Nothing
 where
  itis  = t =~ regex :: Bool
  regex = [r|\`[0-9]+\'|] :: String

nameValidator :: Text -> Maybe Text
nameValidator t = case (onlyAllowedChars, isAllCaps, isAllLower) of
  (False, _    , _    ) -> Nothing
  (True , True , _    ) -> Just . T.toTitle . T.strip $ t -- Is all caps, reCase
  (True , _    , True ) -> Just . T.toTitle . T.strip $ t -- is all lower, reCase
  (True , False, False) -> Just . T.strip $ t             -- user has own casing, keep it
 where
  regex            = [r|\`([^@\.])+\'|] :: String
  onlyAllowedChars = T.strip t =~ regex
  isAllCaps        = T.toUpper t == t
  isAllLower       = T.toLower t == t
