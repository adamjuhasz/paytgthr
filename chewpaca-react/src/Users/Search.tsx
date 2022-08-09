import { FormEvent } from "react";
import { useNavigate } from "react-router-dom";

import { path as userViewPath } from "../User/UserView";

export const path = "/users/search";

interface OurFormElements extends HTMLFormControlsCollection {
  q: HTMLInputElement;
}

interface OurFormElement extends HTMLFormElement {
  readonly elements: OurFormElements;
}

export default function UsersSearch(): JSX.Element {
  const navigate = useNavigate();

  const searchInfo = (form: string) => (event: FormEvent<OurFormElement>) => {
    event.preventDefault();
    const elements = event.currentTarget.elements;
    const q = elements.q.value;
    const isUUID =
      /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/;
    const isUser = isUUID.test(q);
    if (isUser) {
      return navigate(userViewPath.replace(":uid", q));
    }
    navigate({
      pathname: `/users/search/${form}`,
      search: "?" + new URLSearchParams({ q: q }).toString(),
    });
  };

  return (
    <div className="px-2 pt-4 space-y-8 divide-y divide-gray-200">
      <div className="space-y-8 divide-y divide-gray-200 sm:space-y-5">
        <div>
          <div>
            <h3 className="text-lg leading-6 font-medium text-gray-900">
              Find Users
            </h3>
            <p className="mt-1 max-w-2xl text-sm text-gray-500">
              Much finding available
            </p>
          </div>

          <div className="mt-6 sm:mt-5 space-y-6 sm:space-y-5">
            <form
              className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5"
              onSubmit={searchInfo("text")}
            >
              <label
                htmlFor="q"
                className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
              >
                User information seach
                <p className="mt-2 text-sm text-gray-500 font-normal">
                  Does a full text search of user attribures as "contains"
                </p>
              </label>
              <div className="mt-1 sm:mt-0 sm:col-span-2">
                <div className="max-w-lg flex rounded-md shadow-sm">
                  <input
                    type="text"
                    name="q"
                    autoComplete="none"
                    autoCapitalize="none"
                    className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-md sm:text-sm border-gray-300"
                  />
                  <button
                    name="sub"
                    type="submit"
                    className="ml-3 inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    Search
                  </button>
                </div>
              </div>
            </form>
          </div>

          <div className="mt-6 sm:mt-5 space-y-6 sm:space-y-5">
            <form
              className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5"
              onSubmit={searchInfo("address")}
            >
              <label
                htmlFor="q"
                className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
              >
                Address only search
                <p className="mt-2 text-sm text-gray-500 font-normal">
                  Does a full text "contains" search of Street, Street 2, City,
                  State, Zip
                </p>
              </label>
              <div className="mt-1 sm:mt-0 sm:col-span-2">
                <div className="max-w-lg flex rounded-md shadow-sm">
                  <input
                    type="text"
                    name="q"
                    autoComplete="none"
                    className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-md sm:text-sm border-gray-300"
                  />
                  <button
                    name="sub"
                    type="submit"
                    className="ml-3 inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    Search
                  </button>
                </div>
              </div>
            </form>
          </div>

          <div className="mt-6 sm:mt-5 space-y-6 sm:space-y-5">
            <form
              className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5"
              onSubmit={searchInfo("banking")}
            >
              <label
                htmlFor="q"
                className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
              >
                Banking details search
                <p className="mt-2 text-sm text-gray-500 font-normal">
                  Does a full text "contains" search of Street, Street 2, City,
                  State, Zip
                </p>
              </label>
              <div className="mt-1 sm:mt-0 sm:col-span-2">
                <div className="max-w-lg flex rounded-md shadow-sm">
                  <input
                    type="text"
                    name="q"
                    autoComplete="none"
                    className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-md sm:text-sm border-gray-300"
                  />
                  <button
                    name="sub"
                    type="submit"
                    className="ml-3 inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    Search
                  </button>
                </div>
              </div>
            </form>
          </div>
          <></>
        </div>
      </div>
    </div>
  );
}
