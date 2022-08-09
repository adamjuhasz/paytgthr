import {
  CreditCardIcon,
  FingerPrintIcon,
  LibraryIcon,
} from "@heroicons/react/outline";
import PayTgthrDrawing from "./PayTgthrDrawing";

const transferFeatures = [
  {
    id: 1,
    name: "Use your existing bank account",
    description:
      "We link to the checking accounts you already have. There's no account to reload, preload or worry what the balance is.",
    icon: LibraryIcon,
  },
  {
    id: 2,
    name: "An actual Mastercard",
    description:
      "Use it anywhere Mastercard is accepted. Online, swipe, tap, or dip.",
    icon: CreditCardIcon,
  },
  {
    id: 3,
    name: "Change your split anytime",
    description:
      "Everyone is different, change your split anytime! Even change splits for different types of purchases. Split food 70:30 but utilities 50:50.",
    icon: FingerPrintIcon,
  },
];

export default function PayTgthr() {
  return (
    <div
      className="relative mt-12 lg:mt-24 lg:grid lg:grid-cols-2 lg:gap-8 lg:items-center"
      id="pay-tgthr"
    >
      <div className="relative">
        <h2 className="text-2xl font-extrabold text-gray-900 tracking-tight sm:text-3xl">
          Pay Tgthr and the Tgthr Card
        </h2>
        <p className="mt-3 text-lg text-gray-500">
          A new type of debit card that links to each of your existing bank
          accounts
        </p>

        <dl className="mt-10 space-y-10">
          {transferFeatures.map((item) => (
            <div key={item.id} className="relative">
              <dt>
                <div className="absolute flex items-center justify-center h-12 w-12 rounded-md bg-indigo-500 text-white">
                  <item.icon className="h-6 w-6" aria-hidden="true" />
                </div>
                <h2 className="ml-16 text-lg leading-6 font-medium text-gray-900">
                  {item.name}
                </h2>
              </dt>
              <dd className="mt-2 ml-16 text-base text-gray-500">
                {item.description}
              </dd>
            </div>
          ))}
        </dl>
      </div>

      <div className="mt-10 ml-10 mx-4 relative lg:mt-0" aria-hidden="true">
        <svg
          className="invisible lg:visible absolute left-1/2 transform -translate-x-1/2 translate-y-16 lg:hidden"
          width={784}
          height={404}
          fill="none"
          viewBox="0 0 784 404"
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
            width={784}
            height={404}
            fill="url(#00000000-0000-0000-0000-000000000000)"
          />
        </svg>
        <PayTgthrDrawing />
      </div>
    </div>
  );
}
