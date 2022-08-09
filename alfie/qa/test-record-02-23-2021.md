---
title: App testing procedure
---

## Test Information

```
Platform:           "ios"
Platform OS Ver:    ios 14
Device model:       iPhone 12 Pro
App Version:        17.2
Release ID:         71ded0e0-e1f6
Date:               02/23/2021
Tester:             Adam Juhasz
Expo envitonment:   "expo go"
```

## Final Test Score

```
Passed: "Yes" or "No"
Notes:  Yes
```

# Tests

## Login Page

### Test Setup

1. Open storybook using given URL for PR or production
1. Open "Screens" -> "LoginForm" -> "Base State"

### Bad email format

1. Clear all text inputs
1. Enter email using following format
   - (month)(day)-(time)@paytgthr
   - ex: "0319-1402@paytgthr"
1. Enter password
   - "password"
1. Verify email is flagged as malformed
   - Verified: `AJ`
   - Notes: `____`
1. Verify Login and Sign up buttons are disabled
   - Verified: `AJ`
   - Notes: `____`

### Bad password format

1. Clear all text inputs
1. Enter email using following format
   - (month)(day)-(time)@paytgthr.test
   - ex: "0319-1402@paytgthr.test"
1. Enter short password, less than 8 characters
   - ex: "pass"
1. Verify password is flagged as malformed
   - Verified: `AJ`
   - Notes: `____`
1. Verify Login and Sign up buttons are disabled
   - Verified: `AJ`
   - Notes: `____`
1. Contiune typing the password to morethan 8 characters
   - ex: "password"
1. Verify password error goes away
   - Verified: `AJ`
   - Notes: `____`
1. Verify Login and Sign up buttons are now enabled
   - Verified: `AJ`
   - Notes: `____`

### Email suggestion

1. Clear all text inputs
1. Enter email "me@gmail.co"
1. Verify email recomendation is for "me@gmail.com"
   - Verified: `AJ`
   - Notes: `____`
1. Click recomendation
1. Verify email recomendation goes away
   - Verified: `AJ`
   - Notes: `____`
1. Verify email is now the recomendation
   - Verified: `AJ`
   - Notes: `____`

### Test Results

> Test Pass: `Yes` (Yes / No)

## PII Entry Page

### Test Setup

1. Open storybook using given URL for PR or production
1. Open "Screens" -> "SignUp" -> "PII" -> "Base State"

### Formatting checks

1. Clear all text inputs
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Enter "11241985" into DOB
1. Verify formatting of dob into "11/24/1985"
   - Verified: `AJ`
   - Notes: `____`
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Enter "123120000" into SSN
1. Verify formatting into "123-12-000"
   - Verified: `AJ`
   - Notes: `____`
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Enter "2341231234". into Phone
1. Verify formatting into "234-123-1234"
   - Verified: `AJ`
   - Notes: `____`
1. Verify Continue button is enabled
   - Verified: `AJ`
   - Notes: `____`

### DOB malformed with 2 digit year

1. Clear all text inputs
1. Enter "11/11/19" into DOB
1. Enter "123-12-0000" into SSN
1. Enter "234-123-1234" into Phone
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for dob shows up
   - Verified: `AJ`
   - Notes: `____`
1. Enter "11/11/1985" into DOB
1. Verify Continue button is now enabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for dob goes away
   - Verified: `AJ`
   - Notes: `____`

### DOB malformed with bad day

1. Clear all text inputs
1. Enter "11/45/1985" into DOB
1. Enter "123-12-0000" into SSN
1. Enter "234-123-1245" into Phone
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for dob shows up
   - Verified: `AJ`
   - Notes: `____`
1. Enter "11/31/1985" into DOB
1. Verify Continue button is now enabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for dob goes away
   - Verified: `AJ`
   - Notes: `____`

### DOB malformed with bad month

1. Clear all text inputs
1. Enter "13/11/1985" into DOB
1. Enter "123-12-0000" into SSN
1. Enter "234-123-1234" into Phone
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for dob shows up
   - Verified: `AJ`
   - Notes: `____`
1. Enter "11/31/1985" into DOB
1. Verify Continue button is now enabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for dob goes away
   - Verified: `AJ`
   - Notes: `____`

### Phone malformed with bad area code

1. Clear all text inputs
1. Enter "11/13/1985" into DOB
1. Enter "123-12-0000" into SSN
1. Enter "123-123-1234" into Phone
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for phone shows up
   - Verified: `AJ`
   - Notes: `____`
1. Enter "234-123-1234" into Phone
1. Verify Continue button is now enabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for phone goes away
   - Verified: `AJ`
   - Notes: `____`

### Empty inputs do not generate errors

1. Clear all text inputs
1. Enter "11/45/1985" into DOB
1. Enter "999-12-0000" into SSN
1. Enter "123-123-1245" into Phone
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for dob shows up
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for ssn shows up
   - Verified: `AJ`
   - Notes: `____`
1. Verify error for phone shows up
   - Verified: `AJ`
   - Notes: `____`
1. Clear all text inputs
1. Verify all warnings go away
   - Verified: `AJ`
   - Notes: `____`

### Test Results

> Test Pass: `Yes` (Yes / No)

## Sign up flow (requires device)

### Test setup

1. Tap screen ten (10) times to open debug screen
1. Tap "Switch cluster to .dev"
1. Verify there is a red ribbon saying "DEV" on the top right
   - Verified: `AJ`
   - Notes: `____`
1. Click on an empty space 10 times quickly
1. Select logout

### Intro testing

1. Clicking through all (3x) intro screens using "Continue" button
1. Click "Sign up now"
1. Verify login/signup screen is shown
   - Verified: `AJ`
   - Notes: `____`
1. Close app (full kill)
1. Open up
1. Verify sign up screen is shown and not intro screen
   - Verified: `AJ`
   - Notes: `____`

### User creation

1. Enter email using following format
   - (month)(day)-(time)@paytgthr.test
   - "0319-1402@paytgthr.test"
1. Enter password
   - ex: "password"
1. Click Sign up
1. Verify there are no errors
   - Verified: `AJ`
   - Notes: `____`

### Verify navigation menu

1. Verify top left icon is three lines (hamburger)
   - Verified: `AJ`
   - Notes: `____`
1. Click icon
1. Verify sidebar opens and shows with list of signup steps
   - Verified: `AJ`
   - Notes: `____`
1. Verify only steps have no checkmarks
   - Verified: `AJ`
   - Notes: `____`

### Verify state recovery

1. Close & kill application
1. Open application
1. Verify name screen is shown
   - Verified: `AJ`
   - Notes: `____`

### Name Entry

1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Tap the first name input field
1. Verify a normal keyboard is shown
   - Verified: `AJ`
   - Notes: `Keyboard shows name autofill`
1. Verify the Continue button is shown above the keyboard
   - Verified: `AJ`
   - Notes: `____`
1. Verify Continue is disabled
   - Verified: `AJ`
   - Notes: `____`
1. Enter last name
1. Verify Continue is enabled
   - Verified: `AJ`
   - Notes: `____`

### Verify navigation menu

1. Verify top left icon is three lines (hamburger)
   - Verified: `AJ`
   - Notes: `____`
1. Click icon
1. Verify sidebar shows with list of signup steps
   - Verified: `AJ`
   - Notes: `____`
1. Verify "Name entry" has a checkmark
   - Verified: `AJ`
   - Notes: `____`
1. Verify tapping the right side of the screen closes the sidebar
   - Verified: `AJ`
   - Notes: `____`

### Verify state recovery

1. Close & kill application
1. Open application
1. Verify group screen is shown
   - Verified: `AJ`
   - Notes: `____`

### Group creation

1. Verify empty state shows explainer text
   - Verified: `AJ`
   - Notes: `____`
1. Click "Create new group"
1. Click "Cancel"
1. Verify create group screeen is shown
   - Verified: `____`
   - Notes: `____`
1. Tap first name input control
1. Verify normal keyboard is shown
   - Verified: `AJ`
   - Notes: `____`
1. Verify buttons are shown above the keyboard
   - Verified: `AJ`
   - Notes: `____`
1. Enter a first name
   - ex: "Jane"
1. Enter an email in the followin format
   - (month)(day)-(time)@paytgthr.test
   - "0319-1402@paytgthr.test"
1. Click "Create group"
1. Verify there are no errors
   - Verified: `AJ`
   - Notes: `____`

### Verify navigation menu

1. Verify top left icon is three lines (hamburger)
   - Verified: `AJ`
   - Notes: `____`
1. Click icon
1. Verify sidebar shows with list of signup steps
   - Verified: `AJ`
   - Notes: `____`
1. Verify "name entry" has a checkmark next to it
   - Verified: `AJ`
   - Notes: `____`
1. Verify tapping the right side of the screen closes the sidebar
   - Verified: `AJ`
   - Notes: `____`

### Verify state recovery

1. Close & kill application
1. Open application
1. Verify create group screen is shown
   - Verified: `AJ`
   - Notes: `____`
1. Verify user is prompted to set split for group
   - Verified: `AJ`
   - Notes: `____`
1. Verify Continue button is disabled
   - Verified: `AJ`
   - Notes: `____`

### Ratio selection

1. Tap "Set Split"
1. Move the ratio slider up and down
1. Verify percent indicator changes
   - Verified: `AJ`
   - Notes: `____`
1. Verify maximum is 90
   - Verified: `AJ`
   - Notes: `____`
1. Verify minimum is 10
   - Verified: `AJ`
   - Notes: `____`
1. Verify slider follows finger
   - Verified: `AJ`
   - Notes: `____`
1. Verify slider has steps of 5%
   - Verified: `AJ`
   - Notes: `____`
1. Click Continue
1. Verify group says "Still waiting on ..."
   - Verified: `AJ`
   - Notes: `____`
1. Ckick Continue

### Verify navigation menu

1. Verify top left icon is three lines (hamburger)
   - Verified: `AJ`
   - Notes: `____`
1. Click icon
1. Verify sidebar shows with list of signup steps
   - Verified: `AJ`
   - Notes: `____`
1. Verify only steps "name" and "partner" have checkmarks
   - Verified: `AJ`
   - Notes: `____`
1. Verify tapping the right side of the screen closes the sidebar
   - Verified: `AJ`
   - Notes: `____`

### Verify state recovery

1. Close & kill application
1. Open application
1. Verify disclosure screen is shown
   - Verified: `AJ`
   - Notes: `____`

### Disclosure acceptance

1. Click Continue

### Verify navigation menu

1. Verify top left icon is three lines (hamburger)
   - Verified: `AJ`
   - Notes: `____`
1. Click icon
1. Verify sidebar shows with list of signup steps
   - Verified: `AJ`
   - Notes: `____`
1. Verify only steps "name" and "partner" have checkmarks
   - Verified: `AJ`
   - Notes: `____`
1. Verify tapping the right side of the screen closes the sidebar
   - Verified: `AJ`
   - Notes: `____`

### Verify state recovery

1. Close & kill application
1. Open application
1. Verify consent screen is shown
   - Verified: `AJ`
   - Notes: `____`

### Consent agreement

1. Click Agree & Continue

### Verify navigation menu

1. Verify top left icon is three lines (hamburger)
   - Verified: `AJ`
   - Notes: `____`
1. Click icon
1. Verify sidebar shows with list of signup steps
   - Verified: `AJ`
   - Notes: `____`
1. Verify only steps "name", "partner", and "legal" have checkmarks
   - Verified: `AJ`
   - Notes: `____`
1. Verify tapping the right side of the screen closes the sidebar
   - Verified: `AJ`
   - Notes: `____`

### Verify state recovery

1. Close & kill application
1. Open application
1. Verify address screen is shown
   - Verified: `AJ`
   - Notes: `____`

### Address Entry

1. Clear all text inputs
1. Enter "4 heatherwood" into street
1. Enter "Irvine" into city
1. Enter "ZZ" into state
1. Enter "92620" into zip
1. Click Continue
1. Verify recomendation shows up to use "Irvine, CA 92620"
   - Verified: `AJ`
   - Notes: `____`
1. Click "Use new address"

### Verify navigation menu

1. Verify top left icon is three lines (hamburger)
   - Verified: `AJ`
   - Notes: `____`
1. Click icon
1. Verify sidebar shows with list of signup steps
   - Verified: `AJ`
   - Notes: `____`
1. Verify only steps "name", "partner", "legal", and "address" have checkmarks
   - Verified: `AJ`
   - Notes: `____`
1. Verify tapping the right side of the screen closes the sidebar
   - Verified: `AJ`
   - Notes: `____`

### Verify state recovery

1. Close & kill application
1. Open application
1. Verify pii screen is shown
   - Verified: `AJ`
   - Notes: `____`

### PII entry

1. Clear all text inputs
1. Click DOB text field
1. Verify numeric keyboard
   - Verified: `AJ`
   - Notes: `____`
1. Enter "11131985" into DOB
1. Click SSN text field
1. Verify numeric keyboard
   - Verified: `AJ`
   - Notes: `____`
1. Enter "123-12-0000" into SSN
1. Click Phone text field
1. Verify numeric keyboard
   - Verified: `AJ`
   - Notes: `____`
1. Verify phone recomendation shows up
   - Verified: `AJ`
   - Notes: `____`
1. Enter "234-123-1214" into Phone
1. Click Continue

### KYC Passed

1. Veriy screen shows "You're all good"
   - Verified: `AJ`
   - Notes: `____`
1. Click Continue

### Transactions screen

1. Verify first card is "Card disabled", "Partner hasn't signed up", background is yellow
   - Verified: `AJ`
   - Notes: `____`
1. Verify second card is "Bank not linked", "Tap here to link", background is yellow
   - Verified: `AJ`
   - Notes: `____`
1. Verify second card is "Bank not linked", "Tap here to link"
   - Verified: `AJ`
   - Notes: `____`
1. Verify third card is "Activate card", "Tap here to activate"
   - Verified: `AJ`
   - Notes: `____`
1. Tap "Bank not linked"
1. Verify screen is "link account"
   - Verified: `AJ`
   - Notes: `____`
1. Tap back
1. Verify screen is transactions
   - Verified: `AJ`
   - Notes: `____`
1. Tap "Activate card"
1. Verify screen is "activate card"
   - Verified: `AJ`
   - Notes: `____`
1. Tap back
1. Verify screen is transactions
   - Verified: `AJ`
   - Notes: `____`

### Link method selection

1. Click Settings on the botom of the screen
1. Click "Change bank account"
1. Click back
1. Verify screen is settings
   - Verified: `AJ`
   - Notes: `____`
1. Click "Change bank account"
1. Click Direct link
1. Click back
1. Verify screen is link type selection
   - Verified: `AJ`
   - Notes: `____`
1. Click Direct link

### Direct linking

1. Enter a random account name
1. Enter "104102574" for the routing number
1. Enter "123456" for the account number
1. Verify "WAYPOINT BANK" is autofilled for bank name
   - Verified: `AJ`
   - Notes: `____`
1. Click Continue
1. Verify screen is "Will take x days..."
   - Verified: `AJ`
   - Notes: `____`
1. Click Continue
1. Verify screen is transactions
   - Verified: `AJ`
   - Notes: `____`
1. Click Verify bank account
1. Click back
1. Verify screen is transactions
   - Verified: `AJ`
   - Notes: `____`
1. Click Verify bank account
1. Enter values from "0.01" to "0.50" until a value is accepted, clicking Continue after each attempt
1. Verify screen is transactions
   - Verified: `AJ`
   - Notes: `____`
1. Verify Verify bank account card is gone
   - Verified: `AJ`
   - Notes: `____`

### Activate card

1. Click "Activate card" card
1. Enter "1234" as last four
1. Enter matching pins
1. Click Continue
1. Verify screen is transactions
   - Verified: `AJ`
   - Notes: `____`
1. Verify "Activate card" card is gone
   - Verified: `AJ`
   - Notes: `____`

### Test Results

> Test Pass: `Yes` (Yes / No)

## Transactions Screen (requires device)

### Test setup

1. Click on an empty space 10 times quickly
1. Select logout
1. Verify sign up screen is displayed

### Login

1. Enter "00000000-0000-0000-0000-000000000000@paytgthr.test" as the email
1. Enter "password" as the password
1. Clck Continue

### Verify transactions list

1. Verify 5 transactions shown and list is scrollable
   - Verified: `AJ`
   - Notes: `____`
1. Veriy first transaction has pending badge
   - Verified: `AJ`
   - Notes: `____`
1. Verify second transaction has no badges
   - Verified: `AJ`
   - Notes: `____`
1. Verify 3rd transaction has declined badge
   - Verified: `AJ`
   - Notes: `____`
1. Verify 4th transaction has declined badge
   - Verified: `AJ`
   - Notes: `____`
1. Verify 5th transaction has pending badge
   - Verified: `AJ`
   - Notes: `____`

### Test Results

> Test Pass: `Yes` (Yes / No)
