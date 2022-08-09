import { Helmet } from "react-helmet";
import { Link } from "react-router-dom";
import { Post } from "./Types";
import AdamPhoto from "../LandingPage/AdamJuhasz.png";

export const slug = "build-a-card-rewards-program-from-scratch";

export const post: Post = {
  title: "How to build a card rewards program from scratch",
  href: `/blog/${slug}`,
  category: { name: "Behind the scenes" },
  description: "How to build a card rewards program from scratch",
  date: "Nov 28, 2021",
  datetime: "2021-11-28",
  imageUrl: `https://images.unsplash.com/photo-1558939608-7e8f4c8336d2?ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80`,
  imageAlt: "Playing jengo",
  readingTime: "5 min",
  author: {
    name: "Adam Juhasz, CTO",
    href: "#",
    imageUrl: AdamPhoto,
  },
  visble: false,
  reactNode: HowToBuildARewardsProgram,
};

export default post;

function HowToBuildARewardsProgram() {
  return (
    <>
      <Helmet>
        <title>{post.title}</title>
        <meta content={post.description} name="Description" />
      </Helmet>
      <div className="relative py-16 bg-white overflow-hidden">
        <div className="hidden lg:block lg:absolute lg:inset-y-0 lg:h-full lg:w-full">
          <div
            className="relative h-full text-lg max-w-prose mx-auto"
            aria-hidden="true"
          >
            <svg
              className="absolute top-12 left-full transform translate-x-32"
              width={404}
              height={384}
              fill="none"
              viewBox="0 0 404 384"
            >
              <defs>
                <pattern
                  id="00000000-0000-0000-0000-000000000000"
                  x={0}
                  y={0}
                  width={20}
                  height={20}
                  patternUnits="userSpaceOnUse"
                >
                  <rect
                    x={0}
                    y={0}
                    width={4}
                    height={4}
                    className="text-gray-200"
                    fill="currentColor"
                  />
                </pattern>
              </defs>
              <rect
                width={404}
                height={384}
                fill="url(#00000000-0000-0000-0000-000000000000)"
              />
            </svg>
            <svg
              className="absolute top-1/2 right-full transform -translate-y-1/2 -translate-x-32"
              width={404}
              height={384}
              fill="none"
              viewBox="0 0 404 384"
            >
              <defs>
                <pattern
                  id="00000000-0000-0000-0000-000000000000"
                  x={0}
                  y={0}
                  width={20}
                  height={20}
                  patternUnits="userSpaceOnUse"
                >
                  <rect
                    x={0}
                    y={0}
                    width={4}
                    height={4}
                    className="text-gray-200"
                    fill="currentColor"
                  />
                </pattern>
              </defs>
              <rect
                width={404}
                height={384}
                fill="url(#00000000-0000-0000-0000-000000000000)"
              />
            </svg>
            <svg
              className="absolute bottom-12 left-full transform translate-x-32"
              width={404}
              height={384}
              fill="none"
              viewBox="0 0 404 384"
            >
              <defs>
                <pattern
                  id="00000000-0000-0000-0000-000000000000"
                  x={0}
                  y={0}
                  width={20}
                  height={20}
                  patternUnits="userSpaceOnUse"
                >
                  <rect
                    x={0}
                    y={0}
                    width={4}
                    height={4}
                    className="text-gray-200"
                    fill="currentColor"
                  />
                </pattern>
              </defs>
              <rect
                width={404}
                height={384}
                fill="url(#00000000-0000-0000-0000-000000000000)"
              />
            </svg>
          </div>
        </div>
        <div className="relative px-4 sm:px-6 lg:px-8">
          <div className="text-lg max-w-prose mx-auto">
            <h1>
              <span className="block text-base text-center text-rose-600 font-semibold tracking-wide uppercase">
                {post.category.name}
              </span>
              <span className="mt-2 block text-3xl text-center leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
                {post.title}
              </span>
            </h1>
            <p className="mt-8 text-xl text-gray-500 leading-8">
              We always wanted to add a rewards program to the{" "}
              <Link to="/" className="text-rose-500 underline">
                Tgthr Card
              </Link>{" "}
              and to start the process we spent a yeat collecting data on what
              our customers were using the cards to purchase the most often.
            </p>
          </div>
          <div className="mt-6 prose prose-rose prose-lg text-gray-500 mx-auto">
            <h2 className="text-2xl">Table of contents</h2>
            <ul>
              <li>
                <a
                  href="#why-do-we-need-a-rewards-program"
                  className="text-rose-500"
                >
                  Why do we need a rewards program?
                </a>
              </li>
              <li>
                <a href="#points-or-cash-back" className="text-rose-500">
                  Points or cash back?
                </a>
              </li>
              <li>Quantifying risk</li>
              <li>Single or multi-tier?</li>
              <li>Implementing a rewards program</li>
            </ul>
            <p>
              Quis semper vulputate aliquam venenatis egestas sagittis quisque
              orci. Donec commodo sit viverra aliquam porttitor ultrices gravida
              eu. Tincidunt leo, elementum mattis elementum ut nisl, justo,
              amet, mattis. Nunc purus, diam commodo tincidunt turpis. Amet,
              duis sed elit interdum dignissim.
            </p>
            <h2 id="why-do-we-need-a-rewards-program">
              Why do we need a rewards program?
            </h2>
            <p>
              Id orci tellus laoreet id ac. Dolor, aenean leo, ac etiam
              consequat in. Convallis arcu ipsum urna nibh. Pharetra, euismod
              vitae interdum mauris enim, consequat vulputate nibh. Maecenas
              pellentesque id sed tellus mauris, ultrices mauris. Tincidunt enim
              cursus ridiculus mi. Pellentesque nam sed nullam sed diam turpis
              ipsum eu a sed convallis diam.
            </p>
            <h2 id="points-or-cash-back">Points or cash back?</h2>
            <p>
              Faucibus commodo massa rhoncus, volutpat. Dignissim sed eget risus
              enim. Mattis mauris semper sed amet vitae sed turpis id. Id dolor
              praesent donec est. Odio penatibus risus viverra tellus varius sit
              neque erat velit.
            </p>
            <figure>
              <img
                className="w-full rounded-lg"
                src={post.imageUrl}
                alt={post.imageAlt}
                width={1310}
                height={873}
              />
              <figcaption>
                Sagittis scelerisque nulla cursus in enim consectetur quam.
              </figcaption>
            </figure>
            <h2 id="Quantifying risk">Quantifying risk</h2>
            <p>
              Purus morbi dignissim senectus mattis adipiscing. Amet, massa quam
              varius orci dapibus volutpat cras. In amet eu ridiculus leo
              sodales cursus tristique. Tincidunt sed tempus ut viverra
              ridiculus non molestie. Gravida quis fringilla amet eget dui
              tempor dignissim. Facilisis auctor venenatis varius nunc, congue
              erat ac. Cras fermentum convallis quam.
            </p>
            <p>
              Faucibus commodo massa rhoncus, volutpat. Dignissim sed eget risus
              enim. Mattis mauris semper sed amet vitae sed turpis id. Id dolor
              praesent donec est. Odio penatibus risus viverra tellus varius sit
              neque erat velit.
            </p>
          </div>
        </div>
      </div>
    </>
  );
}
