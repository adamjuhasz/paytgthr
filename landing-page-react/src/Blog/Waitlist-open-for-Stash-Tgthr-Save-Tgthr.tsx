import { Post } from "./Types";
import MikePhoto from "../LandingPage/PeterPerez.jpeg";
import { BlogContent, H3, P } from "./BlogContent";

export const post: Post = {
  title: "Waitlist open for Stash Tgthr & Save Tgthr",
  href: "/blog/announcing-stash-tghtr-and-save-tgthr",
  category: { name: "Announcement" },
  description:
    "We're excited to let people start adding themselves to the waitlist for our next two products, Stash Tgthr and Save Tgthr. These new products help couples save short term and long term to build a better foundation together.",
  date: "Sep 16, 2021",
  datetime: "2021-9-16",
  imageUrl:
    "https://images.unsplash.com/photo-1459257831348-f0cdd359235f?ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80",
  imageAlt: "Piggy bank",
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
      intro="We're excited to let people start adding themselves to the
    waitlist for our next two products, Stash Tgthr and Save Tgthr.
    These new products help couples save short term and long term to
    build a better foundation together."
    >
      <H3>Stash Tgthr</H3>
      <P>
        Need to save for that bigger purchase? Stash Tgthr was designed to help
        couples save together for a specific goal. Set a date when you need the
        money by, how much to save and we'll take care of the rest. With a
        combination of rolling up Pay Tgthr purchases, automatic top-ups from
        your linked bank account, and AI smarts we work to make sure you save
        enough to reach your goal.
      </P>
      <P>
        We're excited to see what our customers save for. Some of our beta users
        have already started saving for:
      </P>
      <ul>
        <li>Security deposit</li>
        <li>New couch</li>
        <li>Trip to Mexico</li>
        <li>SpaceX flight</li>
      </ul>
      <H3>Save Tgthr</H3>
      <P>
        Stressed about the future and want to put some cash away for a rainy
        day? Save Tgthr helps couples save together in case of emergencies.
      </P>
    </BlogContent>
  );
}
