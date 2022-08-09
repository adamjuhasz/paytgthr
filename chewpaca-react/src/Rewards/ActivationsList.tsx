import { PropsWithChildren } from "react";
import { clamp } from "lodash";

import { RewardBoostActivation } from "./Types";

import UserLink from "../User/UserLink";
import BoostLink from "../Rewards/BoostLink";
import useCancelBoost from "./UseCancelBoost";
import Spinner from "../Shared/Spinner";

interface Props {
  activations: RewardBoostActivation[];
  group?: string;
}

const Th = (props: PropsWithChildren<unknown>) => (
  <th
    scope="col"
    className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
  >
    {props.children}
  </th>
);

const Td = (props: PropsWithChildren<unknown>) => (
  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
    {props.children}
  </td>
);

const sort = (a: RewardBoostActivation, b: RewardBoostActivation): number => {
  const bEpoch = new Date(b.activationCreatedAt).valueOf();
  const aEpoch = new Date(a.activationCreatedAt).valueOf();

  if (a.activationState === "BoostActive") {
    if (b.activationState === "BoostActive") {
      return clamp(bEpoch - aEpoch, -1, 1);
    }

    return -1;
  }

  if (b.activationState === "BoostActive") {
    return 1;
  }

  return clamp(bEpoch - aEpoch, -1, 1);
};

export default function BoostList({ activations, group }: Props): JSX.Element {
  const cancelBoost = useCancelBoost(group);

  return (
    <div className="flex flex-col max-w-full">
      <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
        <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
          <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <Th>ID</Th>
                  <Th>State</Th>
                  <Th>Uses left</Th>
                  <Th>Reward</Th>
                  <Th>Changed By</Th>
                  <Th>Created</Th>
                  <Th>Expires</Th>
                  <Th>Last Update</Th>
                  <Th>Cancel</Th>
                </tr>
              </thead>
              <tbody>
                {activations.sort(sort).map((boost, entryIdx) => (
                  <tr
                    key={boost.activationId}
                    className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                  >
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      <abbr title={boost.activationId}>
                        {boost.activationId.split("-")[0]}
                      </abbr>
                    </td>
                    <Td>
                      {boost.activationState === "BoostActive" ? (
                        <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-green-100 text-green-800">
                          Active
                        </span>
                      ) : boost.activationState === "BoostUsed" ? (
                        <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-yellow-100 text-yellow-800">
                          Used
                        </span>
                      ) : boost.activationState === "BoostCancelled" ? (
                        <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-purple-100 text-purple-800">
                          Cancelled
                        </span>
                      ) : (
                        <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-gray-100 text-gray-800">
                          {boost.activationState}
                        </span>
                      )}
                    </Td>
                    <Td>
                      {boost.activationUsesLeft === null
                        ? "♾"
                        : boost.activationUsesLeft}
                    </Td>
                    <Td>
                      <BoostLink boostid={boost.activatedReward} />
                    </Td>
                    <Td>
                      <UserLink userid={boost.activationChangeBy} />
                    </Td>
                    <Td>
                      {new Date(boost.activationCreatedAt).toLocaleString()}
                    </Td>
                    <Td>
                      {new Date(boost.activationExpires).toLocaleString()}
                    </Td>
                    <Td>
                      {new Date(boost.activationUpdatedAt).toLocaleString()}
                    </Td>
                    <Td>
                      {boost.activationState === "BoostActive" ? (
                        <button
                          disabled={cancelBoost.isLoading}
                          onClick={() =>
                            cancelBoost.mutateAsync(boost.activationId)
                          }
                          className="justify-center py-0 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white focus:outline-none focus:ring-2 focus:ring-offset-2 bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"
                        >
                          {cancelBoost.isLoading ? (
                            <Spinner />
                          ) : cancelBoost.isError ? (
                            "Error!"
                          ) : (
                            "Cancel"
                          )}
                        </button>
                      ) : (
                        <></>
                      )}
                    </Td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  );
}
