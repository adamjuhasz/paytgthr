import { PropsWithChildren } from "react";
import { Link } from "react-router-dom";

import useGetPrograms from "./UseGetPrograms";

export const path = "/referrals/programs";

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

export default function ListAllReferralPrograms(_props: unknown): JSX.Element {
  const programs = useGetPrograms();

  if (programs.status !== "success") {
    return <div>Loading...</div>;
  }

  return (
    <div className="m-2">
      <div className="flex flex-col max-w-full">
        <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
          <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
            <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <Th>ID</Th>
                    <Th>Active</Th>
                    <Th>Open</Th>
                    <Th>Name</Th>
                    <Th>Last Update</Th>
                  </tr>
                </thead>
                <tbody>
                  {programs.data.map((program, entryIdx) => (
                    <tr
                      key={program.refProgram}
                      className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                    >
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                        <Link
                          to={`/referrals/programs/${program.refProgram}`}
                          className="text-indigo-500"
                        >
                          <abbr title={program.refProgram}>
                            {program.refProgram.split("-")[0]}
                          </abbr>
                        </Link>
                      </td>
                      <Td>
                        {program.refActive ? (
                          <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-green-100 text-green-800">
                            Active
                          </span>
                        ) : (
                          <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-red-100 text-red-800">
                            Not
                          </span>
                        )}
                      </Td>
                      <Td>
                        {program.refOpenAccess ? (
                          <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-green-100 text-green-800">
                            Open
                          </span>
                        ) : (
                          <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-red-100 text-red-800">
                            Not
                          </span>
                        )}
                      </Td>
                      <Td>{program.refName}</Td>
                      <Td>{new Date(program.refUpdatedAt).toLocaleString()}</Td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
