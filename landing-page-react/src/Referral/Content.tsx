import image from "./joel-muniz-ETb_5ayeCik-unsplash.jpg";

export default function ReferralContent() {
  return (
    <div className="py-16 bg-gray-50 overflow-hidden lg:py-24">
      <div className="relative max-w-xl mx-auto px-4 sm:px-6 lg:px-8 lg:max-w-7xl">
        <svg
          className="hidden lg:block absolute left-full transform -translate-x-1/2 -translate-y-1/4"
          width={404}
          height={784}
          fill="none"
          viewBox="0 0 404 784"
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
            height={784}
            fill="url(#00000000-0000-0000-0000-000000000000)"
          />
        </svg>

        <div className="relative">
          <h2 className="text-center text-3xl leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
            Introducing the 5/$20/$20 Referral Program
          </h2>
          <p className="mt-4 max-w-3xl mx-auto text-center text-xl text-gray-500">
            Refer a friend, and when they make 5 purchases of just $20 each,
            you'll each get a $20 credit to your Pay Tgthr account. That’s
            right, refer 5 friends, get $100 in credits!
          </p>
          <p className="mt-4 max-w-3xl mx-auto text-center text-xl text-gray-500">
            Successfully refer more than 5 friends, and we’ll double your reward
            to $40 per referral. There’s no limit to how many referrals you can
            make.
          </p>
        </div>

        <div className="relative mt-12 lg:mt-24 lg:grid lg:grid-cols-2 lg:gap-8 lg:items-center">
          <div className="relative">
            <h3 className="text-2xl font-extrabold text-gray-900 tracking-tight sm:text-3xl">
              How does it work?
            </h3>
            <p className="mt-3 text-lg text-gray-500">
              You know how easy Pay Tgthr makes managing your everyday couple’s
              expenses. You also know couples just like you who could use a
              little help with the same. Let’s spread the good word and help
              your friends and family save time, reduce stress and get a little
              spending money by joining Pay Tgthr!
            </p>

            <dl className="mt-10 space-y-10">
              <div className="relative">
                <dt>
                  <div className="absolute flex items-center justify-center h-12 w-12 rounded-md bg-rose-500 text-white">
                    1
                  </div>
                  <p className="ml-16 text-lg leading-6 font-medium text-gray-900">
                    Invite
                  </p>
                </dt>
                <dd className="mt-2 ml-16 text-base text-gray-500">
                  Invite a couple to sign-up for Pay Tgthr, partner up and link
                  their bank accounts (sorry, but you can’t refer your own
                  partner)
                </dd>
              </div>
              <div className="relative">
                <dt>
                  <div className="absolute flex items-center justify-center h-12 w-12 rounded-md bg-rose-500 text-white">
                    2
                  </div>
                  <p className="ml-16 text-lg leading-6 font-medium text-gray-900">
                    Use
                  </p>
                </dt>
                <dd className="mt-2 ml-16 text-base text-gray-500">
                  They then make 5 purchases of just $20 each using their Tgthr
                  Card within 60 days of account creation.
                </dd>
              </div>
              <div className="relative">
                <dt>
                  <div className="absolute flex items-center justify-center h-12 w-12 rounded-md bg-rose-500 text-white">
                    3
                  </div>
                  <p className="ml-16 text-lg leading-6 font-medium text-gray-900">
                    Profit
                  </p>
                </dt>
                <dd className="mt-2 ml-16 text-base text-gray-500">
                  Both you and the person you refer will get a $20 credit to
                  your Pay Tgthr accounts. Good at getting people to try things
                  that will make their lives better? We’ll double your referral
                  bonus to $40 for every successful referral after your 5th!
                </dd>
              </div>
            </dl>
          </div>

          <div className="mt-10 -mx-4 relative lg:mt-0" aria-hidden="true">
            <svg
              className="absolute left-1/2 transform -translate-x-1/2 translate-y-16 lg:hidden"
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
            <img
              className="relative mx-auto rounded-lg"
              width={490}
              src={image}
              alt=""
            />
          </div>
        </div>

        <svg
          className="hidden lg:block absolute right-full transform translate-x-1/2 translate-y-12"
          width={404}
          height={784}
          fill="none"
          viewBox="0 0 404 784"
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
            height={784}
            fill="url(#00000000-0000-0000-0000-000000000000)"
          />
        </svg>
      </div>
    </div>
  );
}
