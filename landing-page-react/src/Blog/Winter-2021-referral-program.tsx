import { Link } from "react-router-dom";
import { Post } from "./Types";
import MikePhoto from "../LandingPage/PeterPerez.jpeg";
import { BlogContent, P, H3 } from "./BlogContent";

export const post: Post = {
  title: "The 5/$20/$20 Referral Program",
  href: "/blog/winter-2021-referral-program",
  category: { name: "Announcement" },
  description:
    "Pay Tgthr customers can now earn $20 (and even $40) for each couple they successfully refer to Pay Tgthr!",
  date: "Dec 15, 2021",
  datetime: "2021-12-15",
  imageUrl:
    "https://images.unsplash.com/photo-1565688842882-e0b2693d349a?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1170&q=80",
  imageAlt: "5/$20/$20 Referral Program",
  readingTime: "2 min",
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
      intro="Pay Tgthr users know how the Tgthr Card has dramatically reduced the amount of 
    time they spend each day, week and month dealing with their joint expenses. 
    With the Tgthr Card, there’s no need for receipt saving, spreadsheet updating or Venmo requesting. 
    Simply swipe, split and be done. Oh yeah, and earn cash back rewards just for working
    Tgthr! Now, our customers can help their friends, family and 
    acquaintances feel the same stress relief they experience every day (and collect 
    some cash along the way)!"
    >
      <H3>
        How does the <Link to="/web/referral">5/$20/$20 Referral Program</Link>{" "}
        work?
      </H3>

      <P>
        <ol>
          <li>
            Invite your friends, family and acquaintances to register a Pay
            Tgthr account using your unique referral code
          </li>
          <li>The person you refer partners up with their person of choice</li>
          <li>
            They make 5 purchases of just $20 each within 60 days of registering
            an account
          </li>
          <li>Profit (you both get $20 to spend on the Tgthr Card)!</li>
        </ol>
        <b>Know a lot of people who could use Pay Tgthr?</b> If you refer 5
        couples who satisfy the referral criteria, we'll double your bonus to{" "}
        <b>$40</b> for each successful referral after that!{" "}
        <i>There's no limit to how many referrals you can make!</i>
        <br></br>
        <br></br>
        Visit our <Link to="/web/referral">Referral Program</Link> page for more
        details and some FAQs.
      </P>

      <H3>How do I make a referral?</H3>
      <P>
        <ol>
          <li>
            Open the Pay Tgthr app and navigate to the{" "}
            <Link to="/app/settings">Settings</Link> screen
          </li>
          <li>
            Tap <Link to="/app/referral/code">Share your referral code</Link>
          </li>
          <li>
            Share your code with friends, family and acquaintances via text,
            email, carrier pigeon, you name it
          </li>
          <li>
            Remind the person you are referring to enter your referral code
            during sign-up to make sure you both get credit
          </li>
        </ol>
      </P>

      <P>
        At Pay Tgthr, we strive to take as much of the stress and frustration
        related to couple's finances out of your relationship as we can. With{" "}
        <b>your</b> help, we can accomplish this goal for thousands of people
        just like you and revolutionize the way couples bank Tgthr!
      </P>

      <P>
        As always, if you have any questions about any and all things Pay Tgthr,
        drop us a line at <a href="mailto:hi@paytgthr.com">hi@paytgthr.com</a>.
        Click{" "}
        <a href="https://paytgthr.com/legal/referral-winter-21.pdf">here</a> to
        read the full Terms and Conditions.
      </P>
    </BlogContent>
  );
}
