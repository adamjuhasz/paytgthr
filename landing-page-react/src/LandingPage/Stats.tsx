/* This example requires Tailwind CSS v2.0+ */
export default function Example() {
  return (
    <div className="bg-rose-600">
      <div className="max-w-7xl mx-auto py-12 px-4 sm:py-16 sm:px-6 lg:px-8 lg:py-20">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-3xl font-extrabold text-white sm:text-4xl">
            Trusted by people from all 50 states
          </h2>
        </div>
        <dl className="mt-10 text-center sm:max-w-3xl sm:mx-auto sm:grid sm:grid-cols-3 sm:gap-8">
          <div className="flex flex-col">
            <dt className="order-2 mt-2 text-lg leading-6 font-medium text-rose-200">
              Couples
            </dt>
            <dd className="order-1 text-5xl font-extrabold text-white">
              2,000<span className="font-medium">+</span>
            </dd>
          </div>
          <div className="flex flex-col mt-10 sm:mt-0">
            <dt className="order-2 mt-2 text-lg leading-6 font-medium text-rose-200">
              Purchases
            </dt>
            <dd className="order-1 text-5xl font-extrabold text-white">
              30,000<span className="font-medium">+</span>
            </dd>
          </div>
          <div className="flex flex-col mt-10 sm:mt-0">
            <dt className="order-2 mt-2 text-lg leading-6 font-medium text-rose-200">
              Spent
            </dt>
            <dd className="order-1 text-5xl font-extrabold text-white">
              $900k<span className="font-medium">+</span>
            </dd>
          </div>
        </dl>
      </div>
    </div>
  );
}
