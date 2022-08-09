export const EnStrings = {
  Button: {
    Continue: "Continue",
    SignUpNow: "Sign up now",
    Login: "Login",
    SignUp: "Sign up",
    ForgotPassword: "Forgot password",
  },
  Intro: {
    IntroPartnerBody:
      "Both of you get your own Tgthr debit card and agree on how to split purchases",
    IntroPartnerHeading: "You and your partner",
    IntroShoppingBody:
      "Shop anywhere Mastercard is accepted, which is almost everywhere",
    IntroShoppingHeading: "Shop anywhere",
    IntroSplittingBody:
      "We'll split your purchases and charge your share directly to your bank account",
    IntroSplittingHeading: "We do the hard work",
    MonthlyFeeHeading: "No monthly fee",
    MonthlyFeeBody: "Seriously, the Tgthr card is free",
    HiddenFeesHeading: "No hidden fees",
    HiddenFeesBody: "Zip. Nada. Zilch.",
  },
  LoginForm: {
    ScreenTitle: "Log into account",
    DoubleCheckPassword: "Can you double check your password?",
    PasswordRuleFail: "We require passwords to be at least 8 characters",
    EmailMissing:
      "Can you double check if this is the email you signed up with?",
    buttons: {
      login: "Login",
      resetPassword: "Reset password",
    },
  },
  SignUpForm: {
    ScreenTitle: "Create an account",
    DoubleCheckPassword: "Can you double check your password?",
    PasswordRuleFail: "We require passwords to be at least 8 characters",
    EmailMissing: "Can you double check if this the email you signed up with?",
    buttons: {
      signup: "Start",
    },
  },
  Formatting: {
    EmailSuggest: (email: string): string => `Did you mean ${email}?`,
    EmailFormatFail:
      "This doesn't look like a correctly written email address, can you double check it?",
  },
  Transactions: {
    personalLimit: "Personal limit",
    groupLimit: "Group's limit",
    currentbalance: "Pending payments",
  },
  PasswordReset: {
    title: "Reset password",
    explainer: "Your code has been sent and should arrive within 3 minutes",
    labels: {
      verificationCode: "Verification Code",
      newPassword: "New password",
    },
    buttons: { resetButton: "Reset password" },
  },
  PasswordForgot: {
    title: "Forgot Password",
    explainer:
      "Forgot your password?\n\nWe’ll first need to verify that you’re you by sending a code to either the email address or phone number you signed up.\n\nCan’t remember either one? Give us a shout and we’ll work it out",
    buttons: {
      resetSMS: "Reset using Phone",
      resetEmail: "Reset using Email",
    },
  },
  PasswordCodeSend: {
    title: "Verification Code",
    buttons: {
      sendCode: "Send code",
    },
    labels: {
      email: "Email address",
      sms: "Phone number",
    },
  },
  TransactionsHeaderProps: {
    activateActionSheet: {
      title: "Oops! It looks like you don't have a linked bank account yet",
      message:
        "Before you can activate your Tgthr card, you'll need to link your checking account.",
      buttons: {
        link: "Link account",
        faq: "Why Do I Need To Do This?",
        cancel: "Do it later",
      },
    },
  },
};
