import { PropsWithChildren } from "react";
import { sortBy } from "lodash";

import { currToStr } from "../Shared/Currency";

import useGetAllJournals from "./UseGetAllJournals";

export const path = "/ledger/journals";

export default function AllJournalList(_p: unknown): JSX.Element {
  const journals = useGetAllJournals();

  if (journals.status !== "success") {
    return <div>Loading journal list...</div>;
  }

  const sorted = sortBy(journals.data, ["journalUpdated"]).reverse();

  return (
    <div className="m-2 flex flex-col max-w-full">
      <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
        <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
          <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <Th>ID</Th>
                  <Th>Name</Th>
                  <Th>Type</Th>
                  <Th>Balance</Th>
                  <Th>Pending balance</Th>
                  <Th>Last update</Th>
                  <Th>Last Transaction</Th>
                  <Th>Last Entry</Th>
                </tr>
              </thead>
              <tbody>
                {sorted.map((journal, entryIdx) => (
                  <tr
                    key={journal.journalId}
                    className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                  >
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      <a
                        className="text-rose-500 underline"
                        href={`/ledger/journals/${journal.journalId}`}
                      >
                        <abbr title={journal.journalId}>
                          {journal.journalId.split("-")[0]}
                        </abbr>
                      </a>
                    </td>
                    <Td>{journal.journalName}</Td>
                    <Td>{journal.journalType.tag}</Td>
                    <Td>{currToStr(journal.journalBalance)}</Td>
                    <Td>{currToStr(journal.journalPendingBalance)}</Td>
                    <Td>{new Date(journal.journalUpdated).toLocaleString()}</Td>
                    <Td>{journal.journalTransaction.split("-")[0]}</Td>
                    <Td>{journal.lastJournalEntry.split("-")[0]}</Td>
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
