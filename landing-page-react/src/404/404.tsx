import { Helmet } from "react-helmet";

import MiniLogo from "../Shared/MiniLogo.svg";

/* This example requires Tailwind CSS v2.0+ */
export default function FourOhFour() {
  return (
    <>
      <Helmet>
        <title>404: Page not found, Pay Tgthr</title>
        <meta name="prerender-status-code" content="404"></meta>
      </Helmet>
      <div className="bg-white h-full flex flex-col lg:relative">
        <div className="flex-grow flex flex-col">
          <main className="flex-grow flex flex-col bg-white">
            <div className="flex-grow mx-auto max-w-7xl w-full flex flex-col px-4 sm:px-6 lg:px-8">
              <div className="flex-shrink-0 pt-10 sm:pt-16">
                <a href="/" className="inline-flex">
                  <span className="sr-only">Workflow</span>
                  <img className="h-12 w-auto " src={MiniLogo} alt="" />
                </a>
              </div>
              <div className="flex-shrink-0 my-auto py-16 sm:py-32">
                <h1 className="mt-2 text-4xl font-extrabold text-gray-900 tracking-tight sm:text-5xl">
                  Page not found
                </h1>
                <p className="mt-2 text-base text-gray-500">
                  Sorry, we couldn’t find the page you’re looking for.
                </p>
                <div className="mt-6">
                  <a
                    href="/"
                    className="text-base font-medium text-rose-600 hover:text-rose-500"
                  >
                    Go back home<span aria-hidden="true"> &rarr;</span>
                  </a>
                </div>
              </div>
            </div>
          </main>
          <footer className="flex-shrink-0 bg-gray-50">
            <div className="mx-auto max-w-7xl w-full px-4 py-16 sm:px-6 lg:px-8">
              <nav className="flex space-x-4">
                <a
                  href="mailto:hi@paytgthr.com"
                  className="text-sm font-medium text-gray-500 hover:text-gray-600"
                >
                  Contact Support
                </a>
              </nav>
            </div>
          </footer>
        </div>
        <div className="hidden lg:block lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2">
          <img
            className="absolute inset-0 h-full w-full object-cover"
            src="https://images.unsplash.com/photo-1470847355775-e0e3c35a9a2c?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1825&q=80"
            alt="Lost in the desert"
          />
        </div>
      </div>
    </>
  );
}
