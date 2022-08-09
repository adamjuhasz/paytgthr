import { Helmet } from "react-helmet";
import { Link } from "react-router-dom";
import { Post } from "./Types";

import Adam from "../LandingPage/AdamJuhasz.png";
import Mike from "../LandingPage/PeterPerez.jpeg";

export const post: Post = {
  title: "Get to know the founders",
  href: "/blog/get-to-know-the-founders",
  category: { name: "Founders’ Forum" },
  description:
    "Meet Adam and Mike, the founders of Pay Tgthr! We're friends of over 25 years who have set out to revolutionize joint banking for Millennial and Gen Z partners, starting with automated expense splitting",
  date: "Aug 30, 2021",
  datetime: "2021-08-30",
  imageUrl:
    "https://images.unsplash.com/photo-1521747116042-5a810fda9664??ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80",
  imageAlt: "Lomard street in San Francisco",
  readingTime: "8 min",
  author: {
    name: "Mike & Adam",
    href: "#",
    imageUrl: Mike,
  },
  reactNode: BlogContent,
};

export default post;

function BlogContent() {
  return (
    <>
      <Helmet>
        <title>{post.title}</title>
        <meta content={post.description} name="Description" />
      </Helmet>
      <div className="relative bg-white">
        <div className="lg:absolute lg:inset-0">
          <div className="lg:absolute lg:inset-y-0 lg:left-0 lg:w-1/2">
            <img
              className="h-56 w-full object-cover lg:absolute lg:h-full"
              src={post.imageUrl}
              alt={post.imageAlt}
            />
          </div>
        </div>
        <div className="relative pt-12 pb-16 px-4 sm:pt-16 sm:px-6 lg:px-8 lg:max-w-7xl lg:mx-auto lg:grid lg:grid-cols-2">
          <div className="lg:col-start-2 lg:pl-8">
            <div className="text-base max-w-prose mx-auto lg:max-w-lg lg:ml-auto lg:mr-0">
              <h2 className="leading-6 text-rose-600 font-semibold tracking-wide uppercase">
                {post.category.name}
              </h2>
              <h1 className="mt-2 text-3xl leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
                {post.title}
              </h1>
              <p className="mt-8 text-lg text-gray-500">
                <b>Who are the Founders?</b>
              </p>
              <p className="mt-8 text-lg text-gray-500">
                Meet Adam and Mike, the founders of Pay Tgthr! We're friends of
                over 25 years who have set out to revolutionize joint banking
                for Millennial and Gen Z partners, starting with automated
                expense splitting.
              </p>
              <p className="mt-8 text-lg text-gray-500">
                <img
                  src={Adam}
                  alt="Adam Juhasz"
                  className="float-right rounded-lg mt-2 ml-2 mb-2 w-32 h-auto"
                />
                <a href="mailto:adam@paytgthr.com" className="text-rose-500">
                  Adam
                </a>{" "}
                is a University of Michigan-trained software engineer with a
                passion for solving the toughest problems in the couple’s
                finance space. After 10 years programming lasers used to perform
                eye surgery, Adam decided it was time to apply his talents to
                solving critical joint banking problems facing Millennial and
                Gen Z couples. When he’s not cranking out code, you can find him
                taking his Australian Cattle Dog,{" "}
                <Link
                  className="text-rose-500"
                  to={{ pathname: "/", hash: "zzyzx" }}
                >
                  {" "}
                  Zzyzzx
                </Link>
                , on 20 mile hikes with his wife.
              </p>
              <p className="mt-8 text-lg text-gray-500">
                <img
                  src={Mike}
                  alt="Mike Perez"
                  className="float-left rounded-lg mt-2 mr-2 mb-2 w-32 h-auto"
                />
                <a href="mailto:mike@paytgthr.com" className="text-rose-500">
                  Mike
                </a>{" "}
                is a graduate of the UC Berkeley School of Law and spent nearly
                a decade as a corporate attorney advising startups (fintech and
                otherwise) throughout Silicon Valley and beyond. Now, he’s
                putting his experience and knowledge to work building the future
                of joint banking. When he’s not hustling for the benefit of Pay
                Tgthr customers, Mike can usually be found jamming The Clash,
                Sufjan Stevens and the Beach Boys on Zoe, his Gretsch G2420T.
              </p>
              <p className="mt-8 text-lg text-gray-500">
                <b>Why'd you start Pay Tgthr?</b>
              </p>
              <p className="mt-8 text-lg text-gray-500">
                A few years ago, like many of our users, Adam and Mike were in
                long-term committed relationships with their then-girlfriends,
                now wives. Each had tried different ways of managing their joint
                expenses, but found existing solutions to be imperfect,
                cumbersome and time-consuming.
              </p>
              <p className="mt-8 text-lg text-gray-500">
                Mike and his girlfriend used to collect all their receipts at
                the end of the month, calculate how much each person owed and
                then request (and re-request) Venmo payment. Adam and his
                girlfriend tried using a joint checking account but found it
                time consuming to manage the balance. They tried adding an
                authorized user to a credit card, but this still put them back
                into the end-of-month Venmo situation Mike was in.
              </p>
              <p className="mt-8 text-lg text-gray-500">
                Dissatisfied with their existing options, Adam and Mike set out
                to build their own solution; thus was born Pay Tgthr. Now, two
                dudes from Southern California, now in San Francisco, work
                tirelessly to improve the lives of couples across the United
                States!
              </p>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
