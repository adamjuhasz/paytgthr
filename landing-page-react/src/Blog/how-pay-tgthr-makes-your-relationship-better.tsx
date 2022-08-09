import { Post } from "./Types";
import MikePhoto from "../LandingPage/PeterPerez.jpeg";
import { BlogContent, H3, P } from "./BlogContent";

export const post: Post = {
  title: "How Pay Tgthr makes your relationship better",
  href: "/blog/how-pay-tgthr-makes-your-relationship-better",
  category: { name: "Founders’ Forum" },
  description:
    "According to a recent Business Insider article, 65.6% of Millennials and 56% of Gen Z respondents reported that money causes some or a lot of stress in their relationship",
  date: "Oct 26, 2021",
  datetime: "2021-10-26",
  imageUrl:
    "https://images.unsplash.com/photo-1510932742089-bef92acabb5b??ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80",
  imageAlt: "Couple embracing each other",
  readingTime: "8 min",
  author: {
    name: "Mike Perez, CEO",
    href: "#",
    imageUrl: MikePhoto,
  },
  reactNode: TheBlogPost,
};

export default post;

function TheBlogPost() {
  return (
    <BlogContent
      post={post}
      intro="According to a recent Business Insider article, 65.6% of
    Millennials and 56% of Gen Z respondents reported that money
    causes some or a lot of stress in their relationship. At Pay
    Tgthr, we want to reduce or eliminate financial stress by
    automating each aspect of joint banking, starting with how
    couples spend their money."
    >
      <H3>What's the Tgthr Card?</H3>
      <P>
        The Tgthr Card is a Mastercard debit card that links directly to each of
        yours and your partner’s <b>existing</b> bank accounts. You two select
        how you want to split expenses, and anytime you swipe, dip or tap the
        Tgthr Card, Pay Tgthr will do the math and charge each of you your
        share. No more end-of-month accounting. No more Venmo requests. No need
        to open any new accounts. Just you two living your lives Tgthr!
      </P>
      <H3>What can I do with the Pay Tgthr app?</H3>
      <P>Using the Pay Tgthr app, you and your partner can:</P>
      <ul>
        <li>
          Set or change your Reward Boosts to earn 5%, 3% or 1% cash back on
          your purchases as a couple
        </li>
        <li>
          Set your split ratios (one ratio for all purchases, or sub-split by
          category)
        </li>
        <li>Activate your physical Tgthr Card</li>
        <li>
          Quickly and easily copy and paste your card number and details for
          online purchasing
        </li>
        <li>
          See a list of all the purchases you or your partner made using the
          Tgthr Card, the merchant, the amount, what split ratio applied and
          what you owe
        </li>
        <li>See a list of past and upcoming payments made to Pay Tgthr</li>
        <li>
          Monitor your spending limit and see how many more payments you’ll need
          till your next spending limit increase
        </li>
      </ul>
    </BlogContent>
  );
}
