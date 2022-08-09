import { Link } from "react-router-dom";
import Quote from "./Quote";

export default function Example() {
  return (
    <div className="relative my-16 bg-white overflow-hidden">
      <div className="relative px-4 sm:px-6 lg:px-8">
        <div className="text-lg max-w-prose mx-auto">
          <h1>
            <span className="mt-2 block text-3xl text-center leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
              Introducing the first-of-its-kind rewards program designed
              specifically for couples!
            </span>
          </h1>

          <p className="mt-8 text-xl text-gray-500 leading-8">
            At Pay Tgthr, we believe that sharing a purchase is more than just
            saving money. When you share in the purchase of a dinner out, a
            weekend away, or even groceries for a romantic dinner at home,
            you’re sharing experiences that strengthen your relationship. And we
            want to help.
          </p>
        </div>
        <Quote />
        <div className="mt-6 prose prose-rose prose-lg text-gray-500 mx-auto">
          <p>
            With the Pay Tgthr Couple’s Cash Back Rewards Program
            <Link to="#footnote-1" style={{ textDecoration: "none" }}>
              <sup className="text-rose-600">1</sup>
            </Link>
            , you and your partner can earn
            <Link to="#footnote-2" style={{ textDecoration: "none" }}>
              <sup className="text-rose-600">2</sup>
            </Link>{" "}
            cash back just for doing the things you already love to do Tgthr!
          </p>
        </div>
      </div>
    </div>
  );
}
