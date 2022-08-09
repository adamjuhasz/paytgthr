import { Link } from "react-router-dom";
import { defaultTo } from "lodash";

import { path as userEventsPath } from "./AppEventsForUser";
import { path as deviceEventsPath } from "./AppEventsForDevice";
import UserLink from "../User/UserLink";

interface Props {
  entries: DeviceUser[];
}

export interface DeviceUser {
  device: string;
  user: null | string;
}

export default function DeviceUserTable({ entries }: Props): JSX.Element {
  return (
    <div className="flex flex-col max-w-full">
      <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
        <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
          <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <th
                    scope="col"
                    className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                  >
                    User profile
                  </th>
                  <th
                    scope="col"
                    className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                  >
                    User Events (all devices)
                  </th>
                  <th
                    scope="col"
                    className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                  >
                    Device events (all users)
                  </th>
                </tr>
              </thead>
              <tbody>
                {entries.map((entry, entryIdx) => (
                  <tr
                    key={`${entry.device}-${JSON.stringify(entry.user)}`}
                    className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                  >
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      {entry.user === null ? (
                        ""
                      ) : (
                        <UserLink userid={entry.user} />
                      )}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      <Link
                        to={userEventsPath.replace(
                          ":uid",
                          defaultTo(entry.user, "null")
                        )}
                        className="text-indigo-600 hover:text-indigo-900"
                      >
                        {entry.user === null ? "" : entry.user.split("-")[0]}
                      </Link>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                      <Link
                        to={deviceEventsPath.replace(":did", entry.device)}
                        className="text-indigo-600 hover:text-indigo-900"
                      >
                        {entry.device}
                      </Link>
                    </td>
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
