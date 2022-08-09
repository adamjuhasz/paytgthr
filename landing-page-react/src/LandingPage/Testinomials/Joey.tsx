import joey from "./Joey.png";

export default function Joey() {
  return (
    <div className="py-12 px-4  sm:px-6 md:py-16 md:pr-0 md:pl-10 lg:pl-16">
      <blockquote className="mt-6 md:flex-grow md:flex md:flex-col">
        <div className="relative text-lg font-medium text-white md:flex-grow">
          <svg
            className="absolute top-0 left-0 transform -translate-x-8 -translate-y-2 h-8 w-8 text-white opacity-20"
            fill="currentColor"
            viewBox="0 0 32 32"
          >
            <path d="M9.352 4C4.456 7.456 1 13.12 1 19.36c0 5.088 3.072 8.064 6.624 8.064 3.36 0 5.856-2.688 5.856-5.856 0-3.168-2.208-5.472-5.088-5.472-.576 0-1.344.096-1.536.192.48-3.264 3.552-7.104 6.624-9.024L9.352 4zm16.512 0c-4.8 3.456-8.256 9.12-8.256 15.36 0 5.088 3.072 8.064 6.624 8.064 3.264 0 5.856-2.688 5.856-5.856 0-3.168-2.304-5.472-5.184-5.472-.576 0-1.248.096-1.44.192.48-3.264 3.456-7.104 6.528-9.024L25.864 4z" />
          </svg>
          <p className="relative">
            We use to get into arguments about the math behind splitting. Now I
            don't have to math anymore! Instead we just ask "are we taking the
            share card [Tgthr Card]" when we leave the house.
          </p>
          <svg
            className="absolute bottom-0 right-0 rotate-180 transform translate-x-3 translate-y-2 h-8 w-8 text-white opacity-20"
            fill="currentColor"
            viewBox="0 0 32 32"
          >
            <path d="M9.352 4C4.456 7.456 1 13.12 1 19.36c0 5.088 3.072 8.064 6.624 8.064 3.36 0 5.856-2.688 5.856-5.856 0-3.168-2.208-5.472-5.088-5.472-.576 0-1.344.096-1.536.192.48-3.264 3.552-7.104 6.624-9.024L9.352 4zm16.512 0c-4.8 3.456-8.256 9.12-8.256 15.36 0 5.088 3.072 8.064 6.624 8.064 3.264 0 5.856-2.688 5.856-5.856 0-3.168-2.304-5.472-5.184-5.472-.576 0-1.248.096-1.44.192.48-3.264 3.456-7.104 6.528-9.024L25.864 4z" />
          </svg>
        </div>
        <footer className="mt-8">
          <div className="flex items-start">
            <div className="flex-shrink-0 inline-flex rounded-full border-2 border-white bg-white">
              <img className="h-20 w-20 rounded-full" src={joey} alt="Joey" />
            </div>
            <div className="ml-4 flex flex-col justify-center h-20">
              <div className="text-base font-medium text-white">Joey, 22</div>
              <div className="text-base font-medium text-rose-200">Idaho</div>
            </div>
          </div>
        </footer>
      </blockquote>
    </div>
  );
}
