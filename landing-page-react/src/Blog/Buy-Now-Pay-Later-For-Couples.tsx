import { Link } from "react-router-dom";
import { Post } from "./Types";
import MikePhoto from "../LandingPage/PeterPerez.jpeg";
import { BlogContent, P, H3 } from "./BlogContent";

export const post: Post = {
  title:
    "How Pay Tgthr can help split your buy now pay later purchases as a couple",
  href: "/blog/buy-now-pay-later-for-couples",
  category: { name: "Couples Finances" },
  description:
    "Let’s take a look at a new way to pay for purchases using BNPL and how your Tgthr Card can help you effectively manage your couple’s holiday shopping",
  date: "Nov 11, 2021",
  datetime: "2021-11-11",
  imageUrl:
    "https://images.unsplash.com/photo-1604532081136-b157aa1729e8??ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80",
  imageAlt: "Black friday sale",
  readingTime: "5 min",
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
      intro="With Thanksgiving right around the corner, it’s time to start
    thinking about those Black Friday/Cyber Monday purchases you’ll
    make as a couple for your home, friends and family. This time of
    year, couples are looking to take advantage of deals on big
    ticket purchases, like furniture, electronics and more. Let’s
    take a look at a new way to pay for these purchases, and how
    your Tgthr Card can help you effectively manage your couple’s
    holiday shopping."
    >
      <H3>
        BNPL + Pay Tgthr = More time, smaller payments, and cash back rewards
      </H3>

      <P>
        You’ve probably seen the names of buy now, pay later (BNPL) companies,
        like <a href="https://www.klarna.com/us/">Klarna</a> and{" "}
        <a href="https://www.affirm.com/">Affirm</a>, popping up on the checkout
        pages of your favorite online retailers over the last year. BNPL
        services allow couples to buy higher cost items -- like clothes,
        furniture, electronics, and more -- in installment payments over time,
        rather than paying the full cost up front.
      </P>

      <P>
        BNPL usage has increased{" "}
        <a href="https://blog.adobe.com/en/publish/2021/03/15/adobe-digital-economy-index-covid-19-report.html#gs.f0b1xi">
          215% year-over-year
        </a>
        , and according to{" "}
        <a href="https://www.creditkarma.com/about/commentary/buy-now-pay-later-surges-throughout-pandemic-consumers-credit-takes-a-hit">
          Credit Karma
        </a>
        , nearly one-half of all Americans have used a BNPL service to purchase
        an item they needed. With{" "}
        <a href="https://www.forbes.com/sites/ronshevlin/2021/09/07/buy-now-pay-later-the-new-payments-trend-generating-100-billion-in-sales/?sh=58b9f7852ffe">
          41% of Millennials and 36% of Gen Zers
        </a>{" "}
        reporting using BNPL services in 2021, younger generations are leading
        the charge on point-of-sale financing.
      </P>

      <P>
        Pairing a BNPL service with the <Link to="/">Tgthr Card</Link> allows
        couples to more easily manage those payments. Using Pay Tgthr, couples
        can split the cost of a BNPL installment between them reducing the
        burden on either partner.
      </P>

      <P>
        Are you two thinking of buying a new TV for the apartment? Let’s say the
        TV costs $500 and you purchase it on a 4 payment BNPL installment plan.
        Each installment payment is $125. If you split Tgthr Card purchases
        50/50, then using the Tgthr Card as your repayment option, you’ll each
        only pay $62.50 per installment. What’s more, you’ll both earn{" "}
        <Link to="/web/cashback"> cash back rewards</Link> as a couple at the
        same time!
      </P>

      <P>
        In fact, Pay Tgthr couples have already made more than $5,800 in timely
        installment payments to BNPL services! With Pay Tgthr and BNPL, you get
        more time to pay, smaller payments and earn rewards!
      </P>
    </BlogContent>
  );
}
