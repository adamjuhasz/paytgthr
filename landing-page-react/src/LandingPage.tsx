import { Helmet } from "react-helmet";

import NavBar from "./Shared/Navigation/Nav";
import Hero from "./LandingPage/Hero";
import FAQ from "./Shared/FAQ";
import Stats from "./LandingPage/Stats";
import Testinomials from "./LandingPage/Testinomials";
import Features from "./LandingPage/Features";
import Team from "./LandingPage/Team";
import Footer from "./Shared/Footer";
import FounderVideo from "./LandingPage/FounderVideo";

const faqs = [
  {
    question: "Do I have to load money onto the Tgthr Card?",
    answer:
      "Nope! The Tgthr Card isn't a prepaid card or a checking account. There's nothing to load, reload, or preload. Only when you make a purchase do we transfer money from your checking account.",
  },
  {
    question: "Do my partner and I need to be at the same bank already?",
    answer:
      "Nope! We can link to any US bank account, and you can each keep your account at your current bank.",
  },
  {
    question: "Is this a credit card?",
    answer:
      "Nope! The Tgthr Card is a debit card that links to a checking account you already have. Then whenever a purchase is made, we split the purchase using the split ratio you've agreed upon and charge each person their share of the total.",
  },
  {
    question: "What is this ratio you keep mentioning?",
    answer: [
      "We know every couple is unique and has a different financial situation. We let you two decide what's a fair split for you two.",
      "Let's do an example. Say Avery and Jordan have Tgthr Cards and choose to split their purchases 70% for Avery and 30% for Jordan (70:30). When Jordan buys $100 of groceries at the store, we charge Avery $70 and Jordan $30.",
      "Things change too, so whenever you want you can change the ratio for future purchases.",
      "Back to our example... Jordan just got a raise and so they now set the split ratio to 50:50. Now when Avery uses their Tgthr Card on date night for an $80 meal, we charge Avery $40 and Jordan $40. ",
    ],
  },
  {
    question: "Who can I partner with?",
    answer: [
      "Anyone you want!",
      "Seriously, you don't need to have legal or other official connection to each other. However, you should probably trust the other person since you'll automatically split any purchase on either Tgthr Card.",
    ],
  },
  {
    question: "Something else?",
    answer: [
      "Send us an email at hi@paytgthr.com. We'll talk about anything.",
      "Questions about the Tgthr Card, suggestions for the site, how couples should split expenses, why we did this, how to make your own debit card, surprise us...",
    ],
  },
];

export default function LandingPage() {
  return (
    <>
      <Helmet>
        <title>Cash back rewards for couples</title>
        <meta
          content="The Tgthr Card is new type of debit card that lets couples automatically split their purchases. Link your current checking account to your card, choose a fair way to split purchases and go shopping"
          name="Description"
        />
      </Helmet>
      <div className="bg-white">
        <NavBar />

        <main>
          <Hero />

          <FounderVideo />

          <Stats />

          <Testinomials />

          <Features />

          <FAQ faqs={faqs} />

          <Team />
        </main>

        <Footer />
      </div>
    </>
  );
}
