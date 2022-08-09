/* This example requires Tailwind CSS v2.0+ */
import { CheckIcon } from "@heroicons/react/solid";
import { Link } from "react-router-dom";
const plans = [
  {
    title: "Home life",
    featured: false,
    cashback: 3,
    mainFeatures: [
      { id: 3, value: "Meal kits" },
      { id: 4, value: "Utilities" },
      { id: 5, value: "Cable & satellite" },
      { id: 6, value: "Groceries" },
      { id: 7, value: "Pets" },
    ],
    footnote: "3",
  },
  {
    title: "Date night",
    featured: true,
    cashback: 5,
    mainFeatures: [
      { id: 1, value: "Restaurants & bars" },
      { id: 2, value: "Movie theaters" },
      { id: 3, value: "Food delivery apps" },
      { id: 4, value: "Media subscriptions" },
      { id: 5, value: "Live music & performances" },
      { id: 6, value: "Rideshare & public transit" },
    ],
    footnote: "4",
  },
  {
    title: "Everything else",
    featured: false,
    cashback: 1,
    mainFeatures: [
      { id: 1, value: "Online purchases" },
      { id: 2, value: "Gas stations" },
      { id: 3, value: "Home goods" },
      { id: 4, value: "Daily needs" },
    ],
    footnote: "5",
  },
];

function classNames(...classes: any[]) {
  return classes.filter(Boolean).join(" ");
}

export default function Levels() {
  return (
    <div className="bg-white">
      <div className="relative bg-rose-600">
        {/* Overlapping background */}
        <div
          aria-hidden="true"
          className="hidden absolute bg-white w-full h-6 bottom-0 lg:block"
        />

        <div className="relative max-w-2xl mx-auto pt-16 px-4 text-center sm:pt-32 sm:px-6 lg:max-w-7xl lg:px-8">
          <h1 className="text-4xl font-extrabold tracking-tight text-white sm:text-6xl">
            <span className="block lg:inline">Pick your boosts,&nbsp;</span>
            <span className="block lg:inline">change them anytime</span>
          </h1>
          <p className="mt-4 text-xl text-rose-100">
            Pick 2 boosts for each category and change them anytime you want
          </p>
        </div>

        {/* Cards */}
        <div className="relative mt-8 max-w-2xl mx-auto px-4 pb-8 sm:mt-12 sm:px-6 lg:max-w-7xl lg:px-8 lg:pb-0">
          {/* Decorative background */}
          <div
            aria-hidden="true"
            className="hidden absolute top-4 bottom-6 left-8 right-8 inset-0 bg-rose-700 rounded-tl-lg rounded-tr-lg lg:block"
          />

          <div className="relative md:space-y-6 lg:space-y-0 grid grod-cols-1 lg:grid-cols-3">
            {plans.map((plan) => (
              <div
                key={plan.title}
                className={classNames(
                  plan.featured
                    ? "bg-white ring-2 ring-rose-700 shadow-md order-first md:order-none"
                    : "bg-rose-700 lg:bg-transparent my-2",
                  "pt-6 px-6 pb-3 rounded-lg lg:px-8 lg:pt-12 "
                )}
              >
                <div>
                  <h3
                    className={classNames(
                      plan.featured ? "text-rose-600" : "text-white",
                      "text-sm font-semibold uppercase tracking-wide"
                    )}
                  >
                    {plan.title}{" "}
                    <Link to={`#footnote-${plan.footnote}`}>
                      <sup className="text-xs">{plan.footnote}</sup>
                    </Link>
                  </h3>
                  <div className="flex flex-col items-start sm:flex-row sm:items-center sm:justify-between lg:flex-col lg:items-start">
                    <div className="mt-3 flex items-center">
                      <p
                        className={classNames(
                          plan.featured ? "text-rose-600" : "text-white",
                          "text-4xl font-extrabold tracking-tight"
                        )}
                      >
                        {plan.cashback}%{" "}
                        <span className="font-normal">cash back</span>
                      </p>
                    </div>
                  </div>
                </div>
                <h4 className="sr-only">Features</h4>
                <ul
                  className={classNames(
                    plan.featured
                      ? "border-gray-200 divide-gray-200"
                      : "border-rose-500 divide-rose-500 divide-opacity-75",
                    "mt-7 border-t divide-y lg:border-t-0"
                  )}
                >
                  {plan.mainFeatures.map((mainFeature) => (
                    <li key={mainFeature.id} className="py-3 flex items-center">
                      <CheckIcon
                        className={classNames(
                          plan.featured ? "text-rose-500" : "text-rose-200",
                          "w-5 h-5 flex-shrink-0"
                        )}
                        aria-hidden="true"
                      />
                      <span
                        className={classNames(
                          plan.featured ? "text-gray-600" : "text-white",
                          "ml-3 text-sm font-medium"
                        )}
                      >
                        {mainFeature.value}
                      </span>
                    </li>
                  ))}
                </ul>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}
