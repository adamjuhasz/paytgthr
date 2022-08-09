import { PropsWithChildren } from "react";
import { Link } from "react-router-dom";

import { RewardBoost } from "./Types";

interface Props {
  boosts: RewardBoost[];
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

export default function BoostList({ boosts }: Props): JSX.Element {
  return (
    <div className="flex flex-col max-w-full">
      <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
        <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
          <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <Th>ID</Th>
                  <Th>Active</Th>
                  <Th>Name</Th>
                  <Th>Payout</Th>
                  <Th>Max age</Th>
                  <Th>Last Update</Th>
                  <Th>Uses</Th>
                </tr>
              </thead>
              <tbody>
                {boosts.map((boost, entryIdx) => (
                  <tr
                    key={boost.boostId}
                    className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                  >
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      <Link
                        to={`/rewards/reward/${boost.boostId}`}
                        className="text-indigo-500"
                      >
                        <abbr title={boost.boostId}>
                          {boost.boostId.split("-")[0]}
                        </abbr>
                      </Link>
                    </td>
                    <Td>
                      {boost.boostActive ? (
                        <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-green-100 text-green-800">
                          Yes
                        </span>
                      ) : (
                        <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-red-100 text-red-800">
                          No
                        </span>
                      )}
                    </Td>
                    <Td>{boost.boostName}</Td>
                    <Td>
                      <span className="font-medium">
                        {boost.boostRewardInBips / 100}%
                      </span>{" "}
                      <span className="text-gray-400">
                        ({boost.boostRewardInBips} bips)
                      </span>
                    </Td>
                    <Td>{boost.boostExpiresInHr / 24} days</Td>
                    <Td>{new Date(boost.boostUpdated).toLocaleString()}</Td>
                    <Td>{boost.boostUses === null ? "♾" : boost.boostUses}</Td>
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
