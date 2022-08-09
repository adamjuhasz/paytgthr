/*
  This example requires Tailwind CSS v2.0+ 
  
  This example requires some changes to your config:
  
  ```
  // tailwind.config.js
  module.exports = {
    // ...
    plugins: [
      // ...
      require('@tailwindcss/typography'),
    ],
  }
  ```
*/
export default function Example() {
  return (
    <div className="py-16 xl:py-36 px-4 sm:px-6 lg:px-8 bg-white overflow-hidden">
      <div className="max-w-max lg:max-w-7xl mx-auto">
        <div className="relative z-10 mb-8 md:mb-2 md:px-6">
          <div className="text-base max-w-prose lg:max-w-none">
            <h2 className="leading-6 text-rose-600 font-semibold tracking-wide uppercase">
              Lawyer stuff
            </h2>
          </div>
        </div>
        <div className="relative">
          <svg
            className="hidden md:block absolute top-0 right-0 -mt-20 -mr-20"
            width={404}
            height={384}
            fill="none"
            viewBox="0 0 404 384"
            aria-hidden="true"
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
            className="hidden md:block absolute bottom-0 left-0 -mb-20 -ml-20"
            width={404}
            height={384}
            fill="none"
            viewBox="0 0 404 384"
            aria-hidden="true"
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
          <div className="relative md:bg-white md:p-6">
            <div className="lg:grid lg:grid-cols-2 lg:gap-6">
              <div className="prose prose-rose prose-sm text-gray-500 lg:max-w-none">
                <p>
                  <sup id="footnote-1" className="text-rose-600">
                    1
                  </sup>
                  Terms, conditions and limitations apply. See the full Terms
                  for additional details. Pay Tgthr sponsors, administers and is
                  solely responsible for the program. None of our third-party
                  service providers or affiliates sponsor, endorse or administer
                  the program. The Terms and FAQs are subject to change at any
                  time, without notice. Cash back rewards may be adjusted or
                  forfeited under certain circumstances. Cash back rewards are
                  applied to your account automatically when the final
                  transaction has posted, which may take up to two weeks. You
                  must have a minimum of $10.00 in cash rewards to transfer
                  rewards to your bank account.
                </p>
                <p>
                  <sup id="footnote-2" className="text-rose-600">
                    2
                  </sup>
                  Total cash back rewards are limited to $2,000 per calendar
                  year.
                </p>
                <p>
                  <sup id="footnote-3" className="text-rose-600">
                    3
                  </sup>
                  Eligible purchases for Date Night rewards include,
                  restaurants, bars and fast food; streaming entertainment
                  services; public transit, rideshares and taxi cabs; movie
                  theaters; and live music and performances. You must select the
                  Date Night rewards boosts for which you wish to receive cash
                  back rewards in the app. Spend Boosts must be applied prior to
                  purchase. You may select up to two Date Night boosts to
                  receive cash back rewards at any one time. Boosts may be
                  changed any time.
                </p>
              </div>
              <div className="mt-6 prose prose-rose prose-sm text-gray-500 lg:mt-0">
                <p>
                  <sup id="footnote-4" className="text-rose-600">
                    4
                  </sup>
                  Eligible purchases for Home Life rewards include, groceries,
                  cable/satellite TV and internet; meal kits (e.g., Blue Apron);
                  pet supplies and veterinarians; insurance (auto, home, and
                  renters); and charitable giving. You must select the Home Life
                  rewards boosts you wish to receive cash back rewards in the
                  app. Spend Boosts must be applied prior to purchase. You may
                  select up to two Home Life boosts to receive cash back rewards
                  at any one time. Boosts may be changed any time.
                </p>
                <p>
                  <sup id="footnote-5" className="text-rose-600">
                    5
                  </sup>
                  All eligible purchases not categorized as “Date Night” or
                  “Home Life”.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
