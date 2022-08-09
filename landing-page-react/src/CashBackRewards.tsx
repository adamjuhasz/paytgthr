import { Helmet } from "react-helmet";

import NavBar from "./Shared/Navigation/Nav";
import Footer from "./Shared/Footer";
import FAQ from "./Shared/FAQ";
import Levels from "./Cashback/Levels";
import Content from "./Cashback/Content";
import Legal from "./Cashback/Legal";
import HowItWorks from "./Cashback/HowItWorks";

const faqs = [
  {
    question: "What are Cash Rewards?",
    answer:
      "The Pay Tgthr Couple’s Cash Back Rewards program allows you to earn money on the items you and your partner share everyday! You can earn 1%, 3% or 5% on eligible purchases at the stores you shop at every day. Select your Spending Boosts and upon purchase of the qualifying item, you will begin to accrue Cash Rewards. You can then apply your balance to your Pay Tgthr account to offset any monies owed, or you can transfer the Cash Rewards to your verified bank account.",
  },
  {
    question: "How do I select my Spending Boosts? ",
    answer:
      "In your Pay Tgthr app, tap the Rewards on the bottom bar. Next, select two boosts for each of the “Date Night” and “Home Life” spending categories to earn 5% and 3% cash back on qualifying purchases. Remember, you must select your Spending Boost before you make your qualifying purchase in order to accrue Cash Rewards! ",
  },
  {
    question: "How are Cash Rewards calculated?",
    answer:
      "Your Cash Rewards are calculated by applying the applicable cash back percentage to your share of the eligible purchase. For example, if you and your partner make an eligible purchase in the amount of $100 that receives a 5% Cash Reward and you split your purchase 60/40, you will accrue $3.00 in Cash Rewards ($100 x 60% x 5%) and your partner will accrue $2.00 in Cash Rewards ($100 x 40% x 5%). We provide a summary of pending accrued Cash Rewards; however, Cash Rewards may not be redeemed until the transaction is final, which may take up to two weeks following the initial authorization/purchase.",
  },
  {
    question: "How do I see the Cash Rewards that I’ve accrued?",
    answer:
      "You can view your accrued Rewards in the Rewards section of the Pay Tgthr app.",
  },
  {
    question: "How do I redeem my Cash Rewards? ",
    answer:
      "Accrued Cash Rewards may be redeemed in two ways: (1) applying your rewards to your Pay Tgthr account balance, or (2) transferring your rewards to your linked and verified bank account. You can initiate a balance transfer or a transfer to your bank account in the Pay Tgthr app.",
  },
  {
    question:
      "Are there any restrictions on Cash Rewards redemption or accrual?",
    answer:
      "You must have a minimum of $10.00 in accrued Cash Rewards to be eligible for redemption. You may not accrue more than $2,000 in a calendar year. All redemptions are final. You may not transfer, sell or otherwise dispose of your Cash Rewards to another person, or transfer them to any other Pay Tgthr account.",
  },
  {
    question:
      "How quickly will my balance transfer be applied to my outstanding balance?",
    answer:
      "Balance transfers will be immediately applied to offset your Pay Tgthr account balance. If your then-current account balance is less than the transferred Cash Reward, the Cash Reward so transferred will create a credit in your Pay Tgthr account.",
  },
  {
    question: "How quickly can funds be transferred to my bank account?",
    answer:
      "Pay Tgthr uses best efforts to initiate a requested fund transfer within 1 business day of the request. It can take up to 5 business days for your funds to arrive in your bank account.",
  },
  {
    question: "Do my Cash Rewards expire?",
    answer:
      "Cash Rewards do not expire, but see the Program’s Terms & Conditions for information regarding Cash Reward forfeiture and adjustment.",
  },
  {
    question: "Who do I contact if I have questions about my Cash Rewards?",
    answer: "You can reach us at any time by emailing us at hi@paytgthr.com",
  },
];

export default function CashBackRewards(): JSX.Element {
  return (
    <>
      <Helmet>
        <title>Cash back rewards for couples</title>
        <meta
          content="Get 5% cash back for date night purchases, 3% cash back on home life purchases, and 1% back on everything else"
          name="Description"
        />
      </Helmet>
      <div className="bg-white">
        <NavBar />
        <main>
          <Levels />
          <Content />
          <HowItWorks />
          <FAQ faqs={faqs} />
          <Legal />
        </main>
        <Footer />
      </div>
    </>
  );
}
