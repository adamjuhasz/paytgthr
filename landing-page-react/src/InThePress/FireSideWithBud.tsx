export default function FiresideChatWithBud() {
  return (
    <div className="relative bg-white overflow-hidden">
      <div className="relative pt-6 pb-16 sm:pb-24 lg:pb-32">
        <main className="mt-16 mx-auto max-w-7xl px-4 sm:mt-24 sm:px-6 lg:mt-32">
          <div className="lg:grid lg:grid-cols-12 lg:gap-8">
            <div className="mt-12 relative sm:max-w-lg sm:mx-auto lg:mt-0 lg:max-w-none lg:mx-0 lg:col-span-6 lg:flex lg:items-center">
              <div className="relative mx-auto w-full rounded-lg shadow-lg lg:max-w-md">
                <div className="relative block w-full rounded-lg overflow-hidden">
                  <span className="sr-only">Watch our video to learn more</span>
                  <div className="aspect-w-16 aspect-h-9">
                    <iframe
                      className="w-full h-full"
                      title="A Fireside Chat with the co-founders of Pay Tgthr - Mike Perez and Adam Juhasz"
                      src="https://www.youtube-nocookie.com/embed/EmzDU-vknZ8"
                      frameBorder="0"
                      allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                      allowFullScreen
                    />
                  </div>
                </div>
              </div>
            </div>
            <div className="mt-12 sm:text-center md:max-w-2xl md:mx-auto lg:col-span-6 lg:text-left">
              <h2 id="bud-farquhar-fireside">
                <span className="mt-1 block text-5xl tracking-tight font-extrabold ">
                  <span className=" text-gray-900">
                    <span className=" text-indigo-600">A Fireside Chat</span>{" "}
                    with the co-founders
                  </span>
                </span>
              </h2>
              <p className="mt-3 text-base text-gray-500 sm:mt-5 sm:text-xl lg:text-lg xl:text-xl">
                Bud Farquhar interviews our cofounders about what its like to
                found a startup based on 25 years of friendship.
              </p>
            </div>
          </div>
        </main>
      </div>
    </div>
  );
}
