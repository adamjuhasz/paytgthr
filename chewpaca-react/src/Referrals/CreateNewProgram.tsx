/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Helmet } from "react-helmet";
import { FormEvent, useState } from "react";
import { useNavigate } from "react-router";

import Spinner from "../Shared/Spinner";
import { ReferralPayout, ReferralProgram } from "./Types";
import useNewProgram from "./UseNewProgram";

import { path as listPath } from "./AllPrograms";

export const path = "/referrals/new/program";

export default function CreateNewProgram(_props: unknown): JSX.Element {
  const [createError, setCreateError] = useState(false);
  const mutate = useNewProgram();
  const navigate = useNavigate();

  return (
    <>
      <Helmet>
        <title>Create a new referral program</title>
      </Helmet>
      <form
        className="space-y-8 divide-y divide-gray-200 p-2"
        onSubmit={async (event: FormEvent<HTMLFormElement>) => {
          event.preventDefault();
          setCreateError(false);

          const elements = event.currentTarget.elements as any;
          const name = (elements.name as HTMLInputElement).value;
          const workflowStr = (elements.workflow as HTMLInputElement).value;
          const referrerStr = (elements.referrer as HTMLInputElement).value;
          const referreeStr = (elements.referree as HTMLInputElement).value;
          const isActiveStr = (elements.isActive as HTMLInputElement).value;
          const isOpenStr = (elements.isOpen as HTMLInputElement).value;

          let workflow = {};
          let referrer = [];
          try {
            workflow = JSON.parse(workflowStr);
            referrer = JSON.parse(referrerStr);
          } catch (e) {
            setCreateError(true);
            return;
          }
          const referree: ReferralPayout = JSON.parse(referreeStr);
          const isActive = isActiveStr === "True" ? true : false;
          const isOpen = isOpenStr === "True" ? true : false;

          const now = new Date().toISOString();

          const program: ReferralProgram = {
            refProgram: "00000000-0000-0000-0000-000000000000",
            refWorkflow: workflow,
            referrerReward: referrer,
            refereeReward: referree,
            refActive: isActive,
            refOpenAccess: isOpen,
            refCreatedAt: now,
            refUpdatedAt: now,
            refRevision: 1,
            refName: name,
          };

          await mutate.mutateAsync(program);
          navigate(listPath);
        }}
      >
        <div className="space-y-8 divide-y divide-gray-200 sm:space-y-5">
          <div>
            <div>
              <h3 className="text-lg leading-6 font-medium text-gray-900">
                Create a new referral program
              </h3>
              <p className="mt-1 max-w-2xl text-sm text-gray-500">
                Give money to the peoples
              </p>
            </div>

            <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
              <label
                htmlFor="name"
                className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
              >
                Name
              </label>
              <div className="mt-1 sm:mt-0 sm:col-span-2">
                <div className="max-w-lg flex rounded-md shadow-sm">
                  <input
                    type="text"
                    name="name"
                    id="name"
                    autoComplete="off"
                    className={`flex-1 block w-full min-w-0 rounded-md sm:text-sm placeholder-gray-300 border-gray-300 focus:ring-indigo-500 focus:border-indigo-500`}
                    placeholder="Grow Grow referrals"
                  />
                </div>
              </div>
            </div>

            <div className="mt-6 sm:mt-5 space-y-6 sm:space-y-5">
              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="workflow"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Workflow
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <textarea
                    id="workflow"
                    name="workflow"
                    rows={3}
                    className={`max-w-lg shadow-sm block w-full sm:text-sm border rounded-md placeholder-gray-300 ${
                      createError ? "border-red-300" : "border-gray-300"
                    } focus:ring-indigo-500 focus:border-indigo-500"`}
                    placeholder='{"tag": "PurchaseCount", "qualifyingPurchaseMin": ["USD", 2000, 100], "minCount": 5, "timeLimitDays": 60}'
                    defaultValue='{"tag": "PurchaseCount", "qualifyingPurchaseMin": ["USD", 2000, 100], "minCount": 5, "timeLimitDays": 60}'
                  />
                  <p className="mt-2 text-sm text-gray-500">
                    JSON checked by server before it is allowed
                  </p>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="referrer"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Referrer payout
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <textarea
                    id="referrer"
                    name="referrer"
                    rows={3}
                    className={`max-w-lg shadow-sm block w-full sm:text-sm border rounded-md ${
                      createError ? "border-red-300" : "border-gray-300"
                    }  focus:ring-indigo-500 focus:border-indigo-500"`}
                    placeholder='[[0, ["USD", 2000, 100]], [5, ["USD", 4000, 100]]]'
                    defaultValue='[[0, ["USD", 2000, 100]], [5, ["USD", 4000, 100]]]'
                  />
                  <p className="mt-2 text-sm text-gray-500">
                    JSON checked by server before it is allowed
                  </p>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="referree"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Referree payout
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <div className="max-w-lg flex rounded-md shadow-sm">
                    <span className="inline-flex items-center px-3 rounded-l-md border border-r-0 border-gray-300 bg-gray-50 text-gray-500 sm:text-sm">
                      $
                    </span>
                    <input
                      type="text"
                      name="referree"
                      id="referree"
                      autoComplete="off"
                      className="flex-1 block w-full focus:ring-indigo-500 focus:border-indigo-500 min-w-0 rounded-none rounded-r-md sm:text-sm border-gray-300 placeholder-gray-300"
                      placeholder="20.00"
                      defaultValue="20.00"
                    />
                  </div>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="isActive"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Active
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <select
                    name="isActive"
                    className="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
                  >
                    <option value="True">True</option>
                    <option value="False">False</option>
                  </select>
                  <p className="mt-2 text-sm text-gray-500">
                    Set to disable any new refree using this
                  </p>
                </div>
              </div>

              <div className="sm:grid sm:grid-cols-3 sm:gap-4 sm:items-start sm:border-t sm:border-gray-200 sm:pt-5">
                <label
                  htmlFor="isOpen"
                  className="block text-sm font-medium text-gray-700 sm:mt-px sm:pt-2"
                >
                  Open
                </label>
                <div className="mt-1 sm:mt-0 sm:col-span-2">
                  <select
                    name="isOpen"
                    className="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
                  >
                    <option value="True">True</option>
                    <option value="False">False</option>
                  </select>
                  <p className="mt-2 text-sm text-gray-500">
                    If True then any new referee can join program
                  </p>
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
                createError || mutate.isError
                  ? "bg-red-600 hover:bg-red-700 focus:ring-red-500"
                  : "bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"
              }`}
            >
              {mutate.isLoading ? (
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
