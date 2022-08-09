import { Link } from "react-router-dom";
import { Post } from "./Types";
import MikePhoto from "../LandingPage/PeterPerez.jpeg";
import { BlogContent, H3, P } from "./BlogContent";

export const post: Post = {
  title: "Couple’s Cash Back Rewards are here",
  href: "/blog/couples-cash-back-rewards-are-here",
  category: { name: "Announcement" },
  description:
    "Pay Tgthr is excited to announce the first-of-its-kind cash back rewards program designed specifically with couple’s in mind",
  date: "Oct 14, 2021",
  datetime: "2021-10-14",
  imageUrl:
    "https://images.unsplash.com/photo-1606310261591-d6cb98acde93??ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80",
  imageAlt: "Hundred dollar bill",
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
      intro={
        <P>
          Pay Tgthr is excited to announce the first-of-its-kind cash back
          rewards program designed specifically with couple’s in mind! Starting
          today, you can earn:
          <ul className="li">
            <li>
              <b>5%</b> cash back on date night
            </li>
            <li>
              <b>3%</b> cash back on the things you use at home
            </li>
            <li>
              <b>1%</b> cash back on everything else
            </li>
          </ul>
        </P>
      }
    >
      <H3>How To Start Earning Rewards</H3>
      <ol className="">
        <li>Log into the Pay Tgthr app</li>
        <li>Tap the Rewards icon</li>
        <li>
          Select up to 2 “Date Night” Rewards Boosts for which you’ll earn 5%
          cash back
        </li>
        <li>
          Select up to 2 “Home Life” Rewards Boosts for which you’ll earn 3%
          cash back
        </li>
        <li>
          Use your Tgthr Card anywhere Mastercard is accepted to start earning
          cash back rewards today!
        </li>
      </ol>

      <H3>Learn more!</H3>
      <P>
        To learn more about the Pay Tgthr Couple’s Cash Back Rewards Program
        visit the <Link to="/web/cashback">Cash Back Rewards</Link> section of
        our website today!
      </P>
    </BlogContent>
  );
}
