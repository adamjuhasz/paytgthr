## Test Information

```
Platform:    Android
App Version: 17.2-2006
Release ID:  ____
Date:        02/19/2021
Tester:      Charese Embree
Expo envitonment: expo go
```

## Final Test Score

```
Passed: "Yes" or "No"
Notes:  ____
```

## Test procedure

### Switch to dev cluster

1. Tap screen ten (1. times to open debug screen
1. Select dev cluster

### Intro testing

1. Clicking through all (4x) intro screens using continue
1. Verify signup screen is shown
   - Verified: `CE`
1. Close app (full kill)
1. Open up
1. Verify sign up screen is shown and not intro screen
   - Verified: `CE`

### Sign up flow

1. Test setup
   1. Click on an empty space 10 times quickly
   1. Select logout
1. User creation
   1. Bad email format
      1. Enter email using following format
         - (month)(day)-(time)@paytgthr
         - ex: “0319-1402@paytgthr”
      1. Enter password
         - “password”
      1. Click Sign up
      1. Verify email is flagged as malformed
         - Verified: `CE`
   1. Bad password format
      1. Enter email using following format
         - (month)(day)-(time)@paytgthr.test
         - ex: “0319-1402@paytgthr.test”
      1. Enter short password, less than 8 characters
         - ex: “pass”
      1. Click Sign up
      1. Verify password is flagged as malformed
         - Verified: `CE`
   1. Happy path
      1. Enter email using following format
         - (month)(day)-(time)@paytgthr.test
         - “0319-1402@paytgthr.test”
      1. Enter password
         - ex: “Password”
      1. Click Sign up
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify name screen is shown
      - Verified: `CE`
1. Name Entry
   1. Verify Continue button is disabled
      - Verified: `CE`
   1. Enter first name
   1. Verify Continue is disabled
      - Verified: `CE`
   1. Enter last name
   1. Click Continue
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify partner screen is shown
      - Verified: `CE`
1. Partner Entry
   1. Verify Continue button state
      1. Clear all text inputs
      1. Verify Continue button is disabled
         - Verified: `CE`
      1. Enter random first name
      1. Verify Continue is disabled
         - Verified: `CE`
      1. Enter random email address
      1. Verify Continue is enabled
         - Verified: `CE`
   1. Bad email format
      1. Clear all text inputs
      1. Enter random first name
      1. Enter email address “malfomed@example”
      1. Click Continue button
      1. Verify password error is shown
         - Verified: `CE`
   1. Happy path
      1. Clear all text inputs
      1. Enter random first name
      1. Enter email using following format
         - (month)(day)-(time)@paytgthr.test
         - ex: “0319-1402@paytgthr.test”
      1. Click Continue button
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify ratio screen is shown
      - Verified: `CE`
1. Ratio selection
   1. Move the ratio slider up and down
   1. Verify percent indicator changes
      - Verified: `CE`
   1. Verify names are correct
      - Verified: **FAILED**
      - Testing comment: **The screen only shows “you” on top of the screen and “partner” on the bottom, no specific name** -CE
   1. Verify maximum is 90
      - Verified: `CE`
   1. Verify minimum is 10
      - Verified: `CE`
   1. Click Continue
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify disclosure screen is shown
      - Verified: `CE`
1. Disclosure acceptance
   1. Click Continue
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify consent screen is shown
      - Verified: `CE`
1. Consent agreement
   1. Click Continue
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify pii screen is shown
      - Verified: `CE`
1. PII entry
   1. Formatting checks
      1. Clear all text inputs
      1. Verify Continue button is disabled
         - Verified: `CE`
      1. Click DOB text field
      1. Verify numeric keyboard
         - Verified: `CE`
      1. Enter “13111985" into DOB
      1. Verify formatting of dob into “13/11/1985"
         - Verified: `CE`
      1. Click SSN text field
      1. Verify numeric keyboard
         - Verified: `CE`
      1. Enter “12312001. into SSN
      1. Verify formatting into “123-12-0000"
         - Verified: `CE`
      1. Click Phone text field
      1. Verify numeric keyboard
         - Verified: `CE`
      1. Enter “2341231234" into Phone
      1. Verify formatting into “234-123-1234"
         - Verified: `CE`
      1. Verify Continue button is enabled
         - Verified: `CE`
   1. DOB malformed with 2 digit year
      1. Clear all text inputs
      1. Enter “11/11/85" into DOB
      1. Enter “123-12-0000" into SSN
      1. Enter “234-123-1234" into Phone
      1. Verify Continue button is disabled
         - Verified: `CE`
      1. Verify error for dob shows up
         - Verified: `CE`
   1. DOB malformed with bad day
      1. Clear all text inputs
      1. Enter “11/45/1985" into DOB
      1. Enter “123-12-0000" into SSN
      1. Enter “234-123-1234 into Phone
      1. Click Continue 1. Verify error for dob shows up
         - Verified: `CE`
   1. DOB malformed with bad month
      1. Clear all text inputs
      1. Enter “13/11/1985" into DOB
      1. Enter “123-12-0000" into SSN
      1. Enter “234-123-1234" into Phone
      1. Verify Continue button is disabled
         - Verified: `CE`
      1. Verify error for dob shows up
         - Verified: **Failed**
         - Testing comments: **Nothing shows up** -CE
         - Issue Link: https://github.com/paytgthr/monorepo/issues/572
   1. Phone malformed with bad area code
      1. Clear all text inputs
      1. Enter “11/13/191. into DOB
      1. Enter “123-12-0000" into SSN
      1. Enter “123-123-121. into Phone
      1. Click Continue
      1. Verify error for phone shows up
         - Verified: `CE`
   1. Happy path
      1. Clear all text inputs
      1. Enter “11/13/191. into DOB
      1. Enter “123-12-0000" into SSN
      1. Enter “234-123-1234 into Phone
      1. Click Continue
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify address screen is shown
      - Verified: `CE`
1. Address Entry
   1. Verify state entry
      1. Clear all text inputs
      1. Enter “1. main st” into street
      1. Enter “Irvine” into city
      1. Enter “ZZ” into state
      1. Enter “1231. into zip
      1. Click Continue
      1. Verify State error shows up
         - Verified: **FAILED**
         - Testing comment: **no error for state. Only error reads "Address could not be found"** -CE
   1. Happy path
      1. Clear all text inputs
      1. Enter “1. main st” into street
      1. Enter “Irvine” into city
      1. Enter “CA” into state
      1. Enter “1231. into zip
      1. Click Continue
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify link screen is shown
      - Verified: `CE`
1. Link method selection
   1. Click Direct link
   1. Click Continue
   1. Click Continue
   1. Select a bank
      1. Enter “user_good” into id
      1. Enter “pass_good” into password
      1. Click Continue
      1. Select “Plaid checking”
      1. Allow notifications
1. Verify state recovery
   1. Close & kill application
   1. Open application
   1. Verify kyc screen is shown
      - Verified: `CE`
1. KYC Passed
   1. Veriy screen shows account complete
      - Verified: `CE`

### Transactions Screen

1. Test setup
   1. Click on an empty space 10 times quickly
   1. Select logout
   1. Verify sign up screen is displayed
1. Login
   1. Enter “00000000-0000-0000-0000-000000000000@paytgthr.test” as the email
   1. Enter “password” as the password
   1. Clck Continue
1. Verify transactions list
   1. Verify 5 transactions shown and list is scrollable
      - Verified: `CE`
   1. Veriy first transaction has pending badge
      - Verified: `CE`
   1. Verify second transaction has no badges
      - Verified: `CE`
   1. Verify 3rd transaction has declined badge
      - Verified: `CE`
   1. Verify 4th transaction has declined badge
      - Verified: `CE`
   1. Verify 5th transaction has pending badge
      - Verified: `CE`
1. Verify spendable amount
   1. Verify spendable amount is \$19.40
      - Verified: `CE`

### Settings screen

1. Test setup
   1. Click on an empty space 10 times quickly
   1. Select logout
   1. Verify sign up screen is displayed
1. Login
   1. Enter “00000000-0000-0000-0000-000000000000@paytgthr.test” as the email
   1. Enter “password” as the password
   1. Clck Continue
1. Verify settings screen
1. Click the lower switcher.
1. Verify link account button visible
   - Verified: `CE`

### Address requires verification (real device required)

1. Test setup
   1. Click on an empty space 10 times quickly
   1. Select logout
   1. Verify sign up screen is displayed
   1. Enter email using following format
      1. (month)(day)-(time)@paytgthr.test
      1. “0319-1402@paytgthr.test”
   1. Enter “password” as password
   1. Click Sign up
   1. Enter first name
   1. Enter last name
   1. Click Continue to submit name
   1. Enter random first name
   1. Enter email using following format
      - (month)(day)-(time)@paytgthr.test
      - ex: “0319-1402@paytgthr.test”
   1. Click Continue button to invite partner
   1. Click Continue button to accept raio
   1. Click Continue to accept disclosures
   1. Click Continue to accept agreements
   1. Enter “11/13/191. into DOB
   1. Enter “123-12-0000" into SSN
   1. Enter “234-123-1234 into Phone
   1. Click Continue to submit PII
   1. Enter “1. main st” into street
   1. Enter “Irvine” into city
   1. Enter “CA” into state
   1. Enter “1231. into zip
   1. Click Continue to submit address
   1. Click Direct link
   1. Click Continue
   1. Click Continue
   1. Select a bank
   1. Enter “user_good” into id
   1. Enter “pass_good” into password
   1. Click Continue
   1. Select “Plaid checking”
   1. Allow notifications
   1. Verify KYC address verification required shown
      - Verified: `CE`
   1. Click
