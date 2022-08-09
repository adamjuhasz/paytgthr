{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}

module ValidatorsSpec
  ( spec
  )
where

import           LandingPage.Validators         ( ageVerification
                                                , dateCheck
                                                , emailCheck
                                                , isInteger
                                                , nameValidator
                                                , passwordCheck
                                                , phoneCheck
                                                , poboxCheck
                                                , ssnCheck
                                                , stack
                                                , stateCheck
                                                , zipCheck
                                                )
import           Shared.Models.User             ( Password(..) )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , Spec
                                                )

spec :: Spec
spec = parallel $ do
  describe "phoneCheck" $ do
    it "empty" $ phoneCheck "" `shouldBe` Nothing
    it "234.456.1234" $ phoneCheck "234.456.1234" `shouldBe` Just "2344561234"
    it "(234) 456-1234" $ phoneCheck "(234) 456-1234" `shouldBe` Just
      "2344561234"
    it "234-456-1234" $ phoneCheck "234-456-1234" `shouldBe` Just "2344561234"
    it "234 456 1234" $ phoneCheck "234 456 1234" `shouldBe` Just "2344561234"
    it "2344561234" $ phoneCheck "2344561234" `shouldBe` Just "2344561234"
    it "(234)456 1234" $ phoneCheck "(234)456 1234" `shouldBe` Just "2344561234"
    it "2344561234" $ phoneCheck "2344561234" `shouldBe` Just "2344561234"
    it "+12341231234" $ phoneCheck "+12341231234" `shouldBe` Just "9876541234"
    it "987654123" $ phoneCheck "987654123" `shouldBe` Nothing
    it "(987) 654-123" $ phoneCheck "(987) 654-123" `shouldBe` Nothing
    it "1-987-654-1234" $ phoneCheck "1-987-654-1234" `shouldBe` Just
      "9876541234"
    it "123 456 7890" $ phoneCheck "123 456 7890" `shouldBe` Nothing
    it "023 456 7890" $ phoneCheck "023 456 7890" `shouldBe` Nothing
    it "003 456 7890" $ phoneCheck "003 456 7890" `shouldBe` Nothing
    it "1 203 456 7890" $ phoneCheck "1 203 456 7890" `shouldBe` Just
      "2034567890"
    it "1 111 111 1111" $ phoneCheck "1 111 111 1111" `shouldBe` Nothing

  describe "dateCheck" $ do
    it "empty" $ dateCheck "" `shouldBe` Nothing
    it "08021985" $ dateCheck "08021985" `shouldBe` Just "08/02/1985"
    it "08/02/1985" $ dateCheck "08/02/1985" `shouldBe` Just "08/02/1985"
    it "08-02-1985" $ dateCheck "08-02-1985" `shouldBe` Just "08/02/1985"
    it "08\\02\\1985" $ dateCheck "08\\02\\1985" `shouldBe` Just "08/02/1985"
    it "8-2-1985" $ dateCheck "8-2-1985" `shouldBe` Just "08/02/1985"
    it "8-2-85" $ dateCheck "8-2-85" `shouldBe` Nothing
    it "1985/6/30" $ dateCheck "1985/6/30" `shouldBe` Just "06/30/1985"

  describe "emailCheck" $ do
    describe "rejects" $ do
      it "empty" $ emailCheck "" `shouldBe` Nothing
      it "me@gmail." $ emailCheck "me@gmail." `shouldBe` Nothing
      it "me@.com" $ emailCheck "me@.com" `shouldBe` Nothing
      it "@example.com" $ emailCheck "@example.com" `shouldBe` Nothing
      it "9876sfi" $ emailCheck "9876sfi" `shouldBe` Nothing
      it "example.com"  (emailCheck "skipfish@example.com" `shouldBe` Nothing)
      it "example.org"  (emailCheck "skipfish@example.org" `shouldBe` Nothing)
      it "paytgthr.com" (emailCheck "hi@example.com" `shouldBe` Nothing)
      it "invalid TLDs" $ emailCheck "payme@m.con" `shouldBe` Nothing
      it "no TLD" $ emailCheck "payme@mail" `shouldBe` Nothing

    describe "accepts" $ do
      it "me@example.com"
         (emailCheck "me@example.com" `shouldBe` Just "me@example.com")
      it "me+one-two.secret@gmail1.co"
        $          emailCheck "me+one-two.secret@gmail1.co"
        `shouldBe` Just "me+one-two.secret@gmail1.co"
      it "skipfish@example.com"
        $          emailCheck "skipfish@example.com"
        `shouldBe` Just "skipfish@example.com"
      it "aname89@example.com" $ emailCheck "aname89@example.com" `shouldBe` Just
        "aname89@example.com"
      it "12aname@example.com" $ emailCheck "12aname@example.com" `shouldBe` Just
        "12aname@example.com"
      it "89@example.com" $ emailCheck "89@example.com" `shouldBe` Just "89@example.com"
      it "double period in domain"
        $          emailCheck "name@example.com.au"
        `shouldBe` Just "name@example.com.au"
      it "double period in both name and domain"
        $          emailCheck "name.other@example.com.au"
        `shouldBe` Just "name.other@example.com.au"

    describe "xss" $ do
      it "email at end" $ do
        let
          xss
            = "javascript:xssdetected(253455320652400000936915n)//*/javascript:javascript:\"/*'/*`/*--></noscript></title></textarea></style></template></noembed></script><html+\"+onmouseover=/*%26lt;svg/*/onload=xssdetected(253455320652400000936915n)onload=xssdetected(253455320652400000936915n)//><svg+onload=xssdetected(253455320652400000936915n)><svg+onload=xssdetected(253455320652400000936915n)>*/</style><script>xssdetected(253455320652400000936915n)</script><style>skipfish@example.com"
        emailCheck xss `shouldBe` Nothing
      it "email at start" $ do
        let
          xss
            = "skipfish@example.comjavascript:xssdetected(22204742145240000092915n)//*/javascript:javascript:\"/*'/*`/*--></noscript></title></textarea></style></template></noembed></script><html+\"+onmouseover=/*%26lt;svg/*/onload=xssdetected(22204742145240000092915n)onload=xssdetected(22204742145240000092915n)//><svg+onload=xssdetected(22204742145240000092915n)><svg+onload=xssdetected(22204742145240000092915n)>*/</style><script>xssdetected(22204742145240000092915n)</script><style>"
        emailCheck xss `shouldBe` Nothing

  describe "ssnCheck" $ do
    it "empty" $ ssnCheck "" `shouldBe` Nothing
    it "078-05-1120" $ ssnCheck "078-05-1120" `shouldBe` Just "078051120"
    it "078051120" $ ssnCheck "078051120" `shouldBe` Just "078051120"
    it "078 05 1120" $ ssnCheck "078 05 1120" `shouldBe` Just "078051120"
    it "078-05 1120" $ ssnCheck "078-05 1120" `shouldBe` Just "078051120"
    it "078-05-112" $ ssnCheck "078-05-112" `shouldBe` Nothing
    it "07-805-1120" $ ssnCheck "07-805-1120" `shouldBe` Nothing
    it "07805112" $ ssnCheck "07805112" `shouldBe` Nothing
    it "0780" $ ssnCheck "0780" `shouldBe` Nothing
    it "078-05-11-22" $ ssnCheck "07805112" `shouldBe` Nothing
    -- From https://www.ssa.gov/kc/SSAFactSheet--IssuingSSNs.pdf
    it "ITIN numbers start with 9, SSN does not, ITIN OK"
       (ssnCheck "907-82-5112" `shouldBe` Just "907825112")
    it
      "SSA will not issue SSNs beginning with the number '666' in positions 1 – 3."
      (ssnCheck "666-25-1123" `shouldBe` Nothing)
    it
      "SSA will not issue SSNs beginning with the number '000' in positions 1 – 3"
      (ssnCheck "000-25-1124" `shouldBe` Nothing)
    it "SSA will not issue SSNs with the number '00' in positions 4 – 5."
       (ssnCheck "123-00-1124" `shouldBe` Nothing)
    it "SSA will not issue SSNs with the number '0000' in positions 6 – 9."
       (ssnCheck "123-34-0000" `shouldBe` Nothing)
    it "Allows testing SSN" (ssnCheck "123-12-0000" `shouldBe` Just "123120000")

  describe "passwordCheck" $ do
    it "empty" $ passwordCheck (Password "") `shouldBe` Nothing
    it "password" $ passwordCheck (Password "password") `shouldBe` Just
      (Password "password")
    it "c0mpl3x@youH4Xor"
      $          passwordCheck (Password "c0mpl3x@youH4Xor")
      `shouldBe` Just (Password "c0mpl3x@youH4Xor")
    it "short" $ passwordCheck (Password "short") `shouldBe` Nothing

  describe "stateCheck" $ do
    it "California" $ stateCheck "CA" `shouldBe` Just "CA"
    it "New York" $ stateCheck "NY" `shouldBe` Just "NY"
    it "empty" $ stateCheck "" `shouldBe` Nothing
    it "stacked" $ stateCheck "CACA" `shouldBe` Nothing
    it "spaced" $ stateCheck "C A" `shouldBe` Nothing
    it "no front trim" $ stateCheck " CA" `shouldBe` Nothing
    it "no rear trim" $ stateCheck "CA " `shouldBe` Nothing

  describe "zipCheck" $ do
    it "5 digit" $ zipCheck "12345" `shouldBe` Just "12345"
    it "5 digit + 4" $ zipCheck "12345-9876" `shouldBe` Just "12345"
    it "too short" $ zipCheck "1234" `shouldBe` Nothing
    it "too long" $ zipCheck "123456" `shouldBe` Nothing
    it "empty" $ zipCheck "" `shouldBe` Nothing

  describe "PoboxCheck" $ do
    it "695 CAPON ST NE" $ poboxCheck "695 CAPON ST NE" `shouldBe` Just
      "695 CAPON ST NE"
    it "713 BIGGIN POND RD" $ poboxCheck "713 BIGGIN POND RD" `shouldBe` Just
      "713 BIGGIN POND RD"
    it "123 Poor Box Road" $ poboxCheck "123 Poor Box Road" `shouldBe` Just
      "123 Poor Box Road"
    it "123 Po Box Road #5" $ poboxCheck "123 Po Box Road #5" `shouldBe` Just
      "123 Po Box Road #5"
    it "123 Harpo Box Street"
      $          poboxCheck "123 Harpo Box Street"
      `shouldBe` Just "123 Harpo Box Street"
    it "The Postal Road" $ poboxCheck "The Postal Road" `shouldBe` Just
      "The Postal Road"
    it "123 Some Street" $ poboxCheck "123 Some Street" `shouldBe` Just
      "123 Some Street"
    it "123 Some Street Apt 1"
      $          poboxCheck "123 Some Street Apt 1"
      `shouldBe` Just "123 Some Street Apt 1"
    it "123 Some Street Suite 1"
      $          poboxCheck "123 Some Street Suite 1"
      `shouldBe` Just "123 Some Street Suite 1"
    it "123 Some Street unit 1"
      $          poboxCheck "123 Some Street unit 1"
      `shouldBe` Just "123 Some Street unit 1"
    it "Apt 5" $ poboxCheck "Apt 5" `shouldBe` Just "Apt 5"
    it "# 5" $ poboxCheck "# 5" `shouldBe` Just "# 5"
    it "5 " $ poboxCheck "5 " `shouldBe` Just "5 "
    -- PO Box examples
    it "po 123" $ poboxCheck "po 123" `shouldBe` Nothing
    it "pob 555" $ poboxCheck "pob 555" `shouldBe` Nothing
    it "p.o.b. 555" $ poboxCheck "p.o.b. 555" `shouldBe` Nothing
    it "po box 555    " $ poboxCheck "po box 555    " `shouldBe` Nothing
    it "p.o. box 663" $ poboxCheck "p.o. box 663" `shouldBe` Nothing
    it "P.O. Box #123" $ poboxCheck "P.O. Box #123" `shouldBe` Nothing
    it "P.O. Box 3456" $ poboxCheck "P.O. Box 3456" `shouldBe` Nothing
    it "PO Box 1234" $ poboxCheck "PO Box 1234" `shouldBe` Nothing
    it "PO Box Num 1234" $ poboxCheck "PO Box Num 1234" `shouldBe` Nothing
    it "P O Box 4321" $ poboxCheck "P O Box 4321" `shouldBe` Nothing
    it "Post Office Box 9999"
      $          poboxCheck "Post Office Box 9999"
      `shouldBe` Nothing
    it "Apt 4202 mailbox 205"
      $          poboxCheck "Apt 4202 mailbox 205"
      `shouldBe` Nothing
    it "915 Alper Center Dr Apt 4202 mailbox 205"
      $          poboxCheck "915 Alper Center Dr Apt 4202 mailbox 205"
      `shouldBe` Nothing
    it "1316 Greene St P.O. Box 36"
      $          poboxCheck "1316 Greene St P.O. Box 36"
      `shouldBe` Nothing
    it "899 e 12th st apt 8051 Box 959"
      $          poboxCheck "899 e 12th st apt 8051 Box 959"
      `shouldBe` Nothing
    it "PO box 431" $ poboxCheck "PO box 431" `shouldBe` Nothing
    it "P.O. Box 201 86 Glenn Rd."
      $          poboxCheck "P.O. Box 201 86 Glenn Rd."
      `shouldBe` Nothing
    it "977 Sandbox Rd" $ poboxCheck "977 Sandbox Rd" `shouldBe` Just
      "977 Sandbox Rd"
    it "P.O. Box No 203" $ poboxCheck "P.O. Box No 203" `shouldBe` Nothing
    it "P.O.Box No 203" $ poboxCheck "P.O.Box No 203" `shouldBe` Nothing
    it "123 Poor Box Road" $ poboxCheck "123 Poor Box Road" `shouldBe` Just
      "123 Poor Box Road"
    it "123 Harpo Box Street"
      $          poboxCheck "123 Harpo Box Street"
      `shouldBe` Just "123 Harpo Box Street"
    it "123 Poblano Lane" $ poboxCheck "123 Poblano Lane" `shouldBe` Just
      "123 Poblano Lane"
    it "po #123" $ poboxCheck "po #123" `shouldBe` Nothing
    it "The Postal Road" $ poboxCheck "The Postal Road " `shouldBe` Just
      "The Postal Road "
    it "Box Hill" $ poboxCheck "Box Hill " `shouldBe` Just "Box Hill "

  describe "ageVerification" $ do
    let kindaNow = stringToTime "2020-01-29T00:00:00+00:00"
    it "08/02/1985" $ ageVerification kindaNow "08/02/1985" `shouldBe` Just
      "08/02/1985"
    it "01/28/2002" $ ageVerification kindaNow "01/28/2002" `shouldBe` Just
      "01/28/2002"
    it "01/30/2003" $ ageVerification kindaNow "01/30/2003" `shouldBe` Nothing
    it "01/01/1000" $ ageVerification kindaNow "01/01/1000" `shouldBe` Nothing
    it "05/14/1997" $ ageVerification kindaNow "05/14/1997" `shouldBe` Just
      "05/14/1997"
    it "1997-05-14" $ ageVerification kindaNow "1997-05-14" `shouldBe` Just
      "05/14/1997"

  describe "isInteger" $ do
    it "1234" $ isInteger "1234" `shouldBe` Just "1234"
    it "12a34" $ isInteger "12a34" `shouldBe` Nothing
    it "a34" $ isInteger "a34" `shouldBe` Nothing
    it "1234a" $ isInteger "1234a" `shouldBe` Nothing
    it "12.34" $ isInteger "12.34" `shouldBe` Nothing

  describe "stack" $ do
    let kindaNow = stringToTime "2020-01-29T00:00:00+00:00"
    it "[dateCheck, ageVerification] 08/02/1985"
      $          stack [dateCheck, ageVerification kindaNow] "08/02/1985"
      `shouldBe` Just "08/02/1985"
    it "[dateCheck, ageVerification] 01/30/2003"
      $          stack [dateCheck, ageVerification kindaNow] "01/30/2003"
      `shouldBe` Nothing
    it "[dateCheck, ageVerification] 01/30/03"
      $          stack [dateCheck, ageVerification kindaNow] "01/30/03"
      `shouldBe` Nothing
    it "[dateCheck, ageVerification] 1997-05-14"
      $          stack [dateCheck, ageVerification kindaNow] "1997-05-14"
      `shouldBe` Just "05/14/1997"
    it "[dateCheck, ageVerification] 1997-05-14"
      $          stack [dateCheck, ageVerification kindaNow] "1997-05-14"
      `shouldBe` stack [dateCheck]                           "1997-05-14"

  describe "nameValidator" $ do
    it "rejects empty" $ nameValidator "" `shouldBe` Nothing
    it "rejects all spaces" $ nameValidator "    " `shouldBe` Nothing
    it "allows correct name" $ nameValidator "Adam" `shouldBe` Just "Adam"
    it "lowers all caps" $ nameValidator "ADAM" `shouldBe` Just "Adam"
    it "raises all lower" $ nameValidator "adam" `shouldBe` Just "Adam"
    it "keeps user casing" $ nameValidator "McKinley" `shouldBe` Just "McKinley"
    it "checks no email address is accepted"
      $          nameValidator "adam@example.com"
      `shouldBe` Nothing
    it "Allows hiphen" $ nameValidator "John-Stevens" `shouldBe` Just
      "John-Stevens"
    it "Allows apostropje" $ nameValidator "K'La" `shouldBe` Just "K'La"
    it "Allows space between" $ nameValidator "John Stevens" `shouldBe` Just
      "John Stevens"
    it "Corrects extra spaces" $ nameValidator " John Stevens  " `shouldBe` Just
      "John Stevens"

