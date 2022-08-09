import FintechLogo from "./FintechMagLogo.png";

export default function FintechMagazineRTP(): JSX.Element {
  return (
    <div className="relative bg-white overflow-hidden">
      <div className="relative pt-6 pb-16 sm:pb-24 lg:pb-32">
        <main className="mt-16 mx-auto max-w-7xl px-4 sm:mt-24 sm:px-6 lg:mt-32">
          <div className="lg:grid lg:grid-cols-12 lg:gap-8">
            <div className="mt-12 sm:text-center md:max-w-2xl md:mx-auto lg:col-span-6 lg:text-left">
              <h2 id="fintech-magazine-rtp">
                <span className="block text-sm font-semibold uppercase tracking-wide text-rose-500 sm:text-base lg:text-sm xl:text-base">
                  New!
                </span>
                <span className="mt-1 block text-5xl tracking-tight font-extrabold ">
                  <span className=" text-gray-900">
                    Our CEO talks about the{" "}
                    <span className=" text-indigo-600">future of fintech</span>{" "}
                    after RTP
                  </span>
                </span>
              </h2>
              <p className="mt-3 text-base text-gray-500 sm:mt-5 sm:text-xl lg:text-lg xl:text-xl">
                A chat with{" "}
                <a
                  className="text-rose-500 underline"
                  href="https://fintechmagazine.com/digital-payments/jp-morgan-enables-fintech-firm-even-real-time-payments"
                >
                  Fintech Magazine
                </a>{" "}
                about what real-time payments (RTP) will allow Pay Tgthr to do
                in the future
              </p>
            </div>
            <div className="mt-12 relative sm:max-w-lg sm:mx-auto lg:mt-0 lg:max-w-none lg:mx-0 lg:col-span-6 lg:flex lg:items-center">
              <div className="relative mx-auto w-full lg:max-w-md">
                <a href="https://fintechmagazine.com/digital-payments/jp-morgan-enables-fintech-firm-even-real-time-payments">
                  <img src={FintechLogo} alt="Magazine logo" />
                </a>
              </div>
            </div>
          </div>
        </main>
      </div>
    </div>
  );
}
