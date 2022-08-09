import Screen from "./Screen@1000w.png";

export default function Example() {
  return (
    <div className="bg-white overflow-hidden">
      <div className="relative max-w-7xl mx-auto py-16 px-4 sm:px-6 lg:px-8">
        <div className="mt-8 lg:grid lg:grid-cols-2 lg:gap-8">
          <div className="relative lg:row-start-1 lg:col-start-2">
            <svg
              className="hidden lg:block absolute top-0 right-0 -mt-20 -mr-20"
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
            <div className="relative text-base mx-auto max-w-prose lg:max-w-none">
              <figure>
                <div className="aspect-w-12 aspect-h-7 lg:aspect-none">
                  <img
                    className="rounded-lg object-cover object-center"
                    src={Screen}
                    alt="Screenshot"
                    width={1000}
                    height={1162}
                  />
                </div>
              </figure>
            </div>
          </div>
          <div className="mt-8 lg:mt-0 ">
            <div className="mx-auto text-base max-w-prose lg:grid lg:grid-cols-2 lg:gap-8 lg:max-w-none">
              <div>
                <h3 className="mt-2 text-3xl leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
                  How it works
                </h3>
              </div>
            </div>
            <div className="mt-5 prose prose-indigo text-gray-500 mx-auto lg:max-w-none lg:row-start-1 lg:col-start-1">
              <ol>
                <li>Log into the Pay Tgthr app</li>
                <li>Tap the rewards icon</li>
                <li>
                  Select up to two <q>Date Night</q> boosts to earn 5% cash back
                </li>
                <li>
                  Select up to two <q>Home Life</q> boosts to earn 3% cash back
                </li>
                <li>
                  Use your Tgthr Card anywhere Mastercard is accepted to start
                  earning rewards today!
                </li>
              </ol>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
