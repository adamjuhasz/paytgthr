/* eslint-disable jsx-a11y/anchor-is-valid */

import { Link, useLocation } from "react-router-dom";
import { Helmet } from "react-helmet";
import { kebabCase } from "lodash";

import NavBar from "./Shared/Navigation/Nav";
import Footer from "./Shared/Footer";

import { Post } from "./Blog/Types";
import holidayPost from "./Blog/holiday-shopping-together";
import bnpl from "./Blog/Buy-Now-Pay-Later-For-Couples";
import better from "./Blog/how-pay-tgthr-makes-your-relationship-better";
import rewardsHere from "./Blog/Couples-Cash-Back-Rewards-Are-Here";
import waitlist from "./Blog/Waitlist-open-for-Stash-Tgthr-Save-Tgthr";
import founders from "./Blog/get-to-know-the-founders";
import securing from "./Blog/securing-a-fintech";
import rewards from "./Blog/how-to-build-rewards-from-scratch";
import referral from "./Blog/Winter-2021-referral-program";

export const posts: Post[] = [
  referral,
  rewards,
  securing,
  holidayPost,
  bnpl,
  better,
  rewardsHere,
  waitlist,
  founders,
];

export const path = "/blog";

const setSearch = (qs: string) => (key: string, val: null | string) => {
  const searchParams = new URLSearchParams(qs);

  if (val === null) {
    searchParams.delete(key);
  } else {
    searchParams.set(key, val);
  }

  return searchParams.toString();
};

export default function Blog() {
  const location = useLocation();

  const searchParams = new URLSearchParams(location.search);

  const authorSearch = searchParams.get("author");
  const catSearch = searchParams.get("category");
  const visSearch = searchParams.get("visible");

  const setQS = setSearch(location.search);

  return (
    <>
      <Helmet>
        <title>Better Tgthr</title>
        <meta
          content="A collection of articles from Pay Tgthr to help you live better together"
          name="Description"
        />
      </Helmet>
      <NavBar />
      <div className="relative bg-gray-50 pt-16 pb-20 px-4 sm:px-6 lg:pt-24 lg:pb-28 lg:px-8">
        <div className="absolute inset-0">
          <div className="bg-white h-1/3 sm:h-2/3" />
        </div>
        <div className="relative max-w-7xl mx-auto">
          <div className="text-center">
            <h1 className="text-3xl tracking-tight font-extrabold text-gray-900 sm:text-4xl">
              Better Tgthr, the Pay Tgthr blog
            </h1>
            <p className="mt-3 max-w-2xl mx-auto text-xl text-gray-500 sm:mt-4">
              A few words at a time to help you build a stronger relationship
              together
            </p>
          </div>
          <div className="mt-12 max-w-lg mx-auto grid gap-5 lg:grid-cols-3 lg:max-w-none">
            {posts
              .filter((p) => (visSearch === "all" ? true : p.visble !== false))
              .filter((p) =>
                authorSearch === null ? true : p.author.name === authorSearch
              )
              .filter((p) =>
                catSearch === null ? true : p.category.name === catSearch
              )
              .map((post) => (
                <div
                  key={post.href}
                  id={kebabCase(post.title)}
                  className="flex flex-col rounded-lg shadow-lg overflow-hidden"
                >
                  <div className="flex-shrink-0">
                    <Link to={post.href}>
                      <img
                        key={`${kebabCase(post.title)}-cover`}
                        id={`${kebabCase(post.title)}-cover`}
                        className="h-48 w-full object-cover"
                        src={post.imageUrl}
                        alt={post.imageAlt}
                      />
                    </Link>
                  </div>
                  <div className="flex-1 bg-white p-6 flex flex-col justify-between">
                    <div className="flex-1">
                      <p className="text-sm font-medium text-rose-600">
                        <Link
                          to={{
                            search:
                              catSearch === null
                                ? setQS("category", post.category.name)
                                : setQS("category", null),
                          }}
                          className="hover:underline"
                        >
                          {post.category.name}
                        </Link>
                      </p>
                      <Link to={post.href} className="block mt-2">
                        <p className="text-xl font-semibold text-gray-900">
                          {post.title}
                        </p>
                        <p className="mt-3 text-base text-gray-500">
                          {post.description}
                        </p>
                      </Link>
                    </div>
                    <div className="mt-6 flex items-center">
                      <div className="flex-shrink-0">
                        <Link
                          to={{
                            search:
                              authorSearch === null
                                ? setQS("author", post.author.name)
                                : setQS("author", null),
                          }}
                          className="hover:underline"
                        >
                          <span className="sr-only">{post.author.name}</span>
                          <img
                            key={kebabCase(`${post.title}-${post.author.name}`)}
                            id={kebabCase(`${post.title}-${post.author.name}`)}
                            className="h-10 w-10 rounded-full"
                            src={post.author.imageUrl}
                            alt={post.author.name}
                          />
                        </Link>
                      </div>
                      <div className="ml-3">
                        <p className="text-sm font-medium text-gray-900">
                          <Link
                            to={{
                              search:
                                authorSearch === null
                                  ? setQS("author", post.author.name)
                                  : setQS("author", null),
                            }}
                            className="hover:underline"
                          >
                            {post.author.name}
                          </Link>
                        </p>
                        <div className="flex space-x-1 text-sm text-gray-500">
                          <time dateTime={post.datetime}>{post.date}</time>
                          <span aria-hidden="true">&middot;</span>
                          <span>{post.readingTime} read</span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              ))}
          </div>
        </div>
      </div>
      <Footer />
    </>
  );
}
