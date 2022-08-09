/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-call */

import { PropsWithChildren } from "react";

import { Group } from "./Types";

import UserLink from "../User/UserLink";

interface Props {
  groups: Group[];
  user?: string;
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

export default function DeviceUserTable({ groups, user }: Props): JSX.Element {
  return (
    <div className="flex flex-col max-w-full">
      <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
        <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
          <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <Th>ID</Th>
                  <Th>Revision</Th>
                  <Th>Status</Th>
                  <Th>Members</Th>
                  <Th>Splits</Th>
                </tr>
              </thead>
              <tbody>
                {groups.map((entry, entryIdx) => (
                  <tr
                    key={entry.id}
                    className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                  >
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      <abbr title={entry.id}>{entry.id.split("-")[0]}</abbr>
                    </td>
                    <Td>{entry.revision}</Td>
                    <Td>{entry.status}</Td>
                    <Td>
                      {entry.members.map((member) => (
                        <div>
                          <UserLink userid={member.user} thisUser={user} />
                        </div>
                      ))}
                    </Td>
                    <Td>
                      {entry.split.map((split) => (
                        <div>
                          <span>{split.ratio.numerator}%</span>{" "}
                          <UserLink userid={split.user} thisUser={user} />
                        </div>
                      ))}
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
