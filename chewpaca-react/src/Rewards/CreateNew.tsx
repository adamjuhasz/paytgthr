/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-explicit-any */

import { Helmet } from "react-helmet";
import { FormEvent, useState } from "react";
import { isEqual } from "lodash";
import { useNavigate } from "react-router-dom";

import useCreateNewBoost from "./UseCreateBoost";
import { RewardBoost } from "./Types";
import { path as listBoostsPath } from "./ListBoosts";
import Spinner from "../Shared/Spinner";

export const path = "/rewards/new";

export default function CreateNewBoost(): JSX.Element {
  const [jsonError, setJsonError] = useState(false);
  const [nameError, setNameError] = useState(false);
  const [createError, setCreateError] = useState(false);

  const createNew = useCreateNewBoost();
  const navigate = useNavigate();

  return (
    <>
      <Helmet>
        <title>Create a new boost</title>
      </Helmet>
      <form
        className="space-y-8 divide-y divide-gray-200 p-2"
        onSubmit={async (event: FormEvent<HTMLFormElement>) => {
          event.preventDefault();
          setJsonError(false);
          setNameError(false);
          setCreateError(false);

          const elements = event.currentTarget.elements as any;
          const boostname = (elements.boostname as HTMLInputElement).value;
          const bips = (elements.bips as HTMLInputElement).value;
          const lifetime = (elements.lifetime as HTMLInputElement).value;
          const payout = (elements.payout as HTMLInputElement).value;
          const matcher = (elements.matcher as HTMLInputElement).value;
          const isActive = (elements.isActive as HTMLInputElement).value;
          const maxuses = (elements.maxuses as HTMLInputElement).value;
          let j = {};
          try {
            j = JSON.parse(matcher);
          } catch (e) {
            setJsonError(true);
            return;
          }
          console.log("isActive", isActive);

          const reward: RewardBoost = {
            boostName: boostname,
            boostActive: isActive === "true" ? true : false,
            boostCreated: new Date().toISOString(),
            boostExpiresInHr: parseInt(lifetime, 10),
            boostId: "00000000-0000-0000-0000-000000000000",
            boostMatch: j,
            boostMaximumPayout: ["USD", parseInt(payout, 10), 1],
            boostRewardInBips: parseInt(bips, 10),
            boostUpdated: new Date().toISOString(),
            boostUses:
              parseInt(maxuses, 10) === 0 ? null : parseInt(maxuses, 10),
          };

          if (reward.boostName === "") {
            setNameError(true);
            return;
          }

          if (
            isEqual(reward.boostMatch, {}) ||
            isEqual(reward.boostMatch, [])
          ) {
            setJsonError(true);
            return;
          }
          try {
            await createNew.mutateAsync(reward);
          } catch (e) {
            setCreateError(true);
            return;
          }
          navigate(listBoostsPath);
        }}
      >
        <div className="space-y-8 divide-y divide-gray-200 sm:space-y-5">
          <div>
            <div>
              <h3 className="text-lg leading-6 font-medium text-gray-900">
                Create a new boost
              </h3>
              <p className="mt-1 max-w-2xl text-sm text-gray-500">
                Give money to the peoples
              </p>
            </div>

            <div className="mt-6 sm:mt-5 space-y-6 sm:space-y-5">
              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="boostname"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Boost Name
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <div className="max-w-lg flex rounded-md shadow-sm">
                    <input
                      type="text"
                      name="boostname"
                      id="boostname"
                      autoComplete="off"
                      className={`flex-1 block w-full min-w-0 rounded-md sm:text-sm placeholder-gray-300 ${
                        nameError
                          ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                          : "border-gray-300 focus:ring-indigo-500 focus:border-indigo-500"
                      }`}
                      placeholder="Groceries and food stores"
                    />
                  </div>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="bips"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Reward in bips (100 bips = 1%)
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <div className="max-w-lg flex rounded-md shadow-sm">
                    <input
                      type="text"
                      name="bips"
                      id="bips"
                      autoComplete="off"
                      className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-none rounded-l-md sm:text-sm border-gray-300 placeholder-gray-300"
                      placeholder="100"
                      defaultValue="100"
                    />
                    <span className="inline-flex items-center px-3 rounded-r-md border border-l-0 border-gray-300 bg-gray-50 text-gray-500 sm:text-sm">
                      bips
                    </span>
                  </div>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="lifetime"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Lifetime in hours (8760 hours = 365 days)
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <div className="max-w-lg flex rounded-md shadow-sm">
                    <input
                      type="text"
                      name="lifetime"
                      id="lifetime"
                      autoComplete="off"
                      className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-none rounded-l-md sm:text-sm border-gray-300 placeholder-gray-300"
                      placeholder="8760"
                      defaultValue="8760"
                    />
                    <span className="inline-flex items-center px-3 rounded-r-md border border-l-0 border-gray-300 bg-gray-50 text-gray-500 sm:text-sm">
                      hours
                    </span>
                  </div>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="maxuses"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Max uses (0 is infinite)
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <input
                    type="text"
                    name="maxuses"
                    id="maxuses"
                    autoComplete="off"
                    className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-none rounded-l-md sm:text-sm border-gray-300 placeholder-gray-300"
                    placeholder="0"
                    defaultValue="0"
                  />
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="payout"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Max Payout per purchase in $
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <div className="max-w-lg flex rounded-md shadow-sm">
                    <span className="inline-flex items-center px-3 rounded-l-md border border-r-0 border-gray-300 bg-gray-50 text-gray-500 sm:text-sm">
                      $
                    </span>
                    <input
                      type="text"
                      name="payout"
                      id="payout"
                      autoComplete="off"
                      className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-none rounded-r-md sm:text-sm border-gray-300 placeholder-gray-300"
                      placeholder="1000000"
                      defaultValue="1000000"
                    />
                  </div>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="matcher"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Matcher JSON
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <textarea
                    id="matcher"
                    name="matcher"
                    rows={3}
                    className={`max-w-lg shadow-sm block w-full sm:text-sm border rounded-md placeholder-gray-300 ${
                      jsonError
                        ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                        : "border-gray-300 focus:ring-indigo-500 focus:border-indigo-500"
                    }`}
                    placeholder='{"tag":"MatchOR","contents":[{"tag":"MatchMCC","contents":"6300"},{"tag":"MatchMCC","contents":"6381"}]}'
                  />
                  <p className="mt-2 text-sm text-gray-500">
                    JSON checked by server before it is allowed
                  </p>
                </div>
              </div>
            </div>
          </div>

          <div className="divide-y divide-gray-200 space-y-6 sm:pt-5 sm:space-y-5">
            <div className="space-y-6 sm:space-y-5 divide-y divide-gray-200">
              <div className="sm:pt-2">
                <div role="group" aria-labelledby="label-notifications">
                  <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-baseline">
                    <div>
                      <div
                        className="text-base font-medium text-gray-900 sm:text-sm sm:text-gray-700"
                        id="label-notifications"
                      >
                        Advertised to users publicly?
                      </div>
                    </div>
                    <div className="sm:col-span-2">
                      <div className="max-w-lg">
                        <div className="space-y-4">
                          <div className="flex items-center">
                            <input
                              id="true"
                              name="isActive"
                              type="radio"
                              value="true"
                              className="focus:ring-green-500 h-4 w-4 text-green-600 border-gray-300"
                              defaultChecked
                            />
                            <label
                              htmlFor="true"
                              className="ml-3 block text-sm font-medium text-gray-700"
                            >
                              Advertised
                            </label>
                          </div>
                          <div className="flex items-center">
                            <input
                              id="false"
                              name="isActive"
                              value="false"
                              type="radio"
                              className="focus:ring-red-500 h-4 w-4 text-red-600 border-gray-300"
                            />
                            <label
                              htmlFor="false"
                              className="ml-3 block text-sm font-medium text-gray-700"
                            >
                              Not advertised
                            </label>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div className="pt-5">
          <div className="flex justify-end">
            <button
              type="submit"
              className={`ml-3 inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white focus:outline-none focus:ring-2 focus:ring-offset-2 ${
                createError
                  ? "bg-red-600 hover:bg-red-700 focus:ring-red-500"
                  : "bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"
              }`}
            >
              {createNew.isLoading ? (
                <Spinner />
              ) : createError ? (
                "Error"
              ) : (
                "Create"
              )}
            </button>
          </div>
        </div>
      </form>
    </>
  );
}
