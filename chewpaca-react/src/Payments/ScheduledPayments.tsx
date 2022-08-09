import { MouseEventHandler, PropsWithChildren, useState } from "react";
import { orderBy } from "lodash";

import UserLink from "../User/UserLink";
import useGetScheduledPayments from "./UseGetScheduledPayments";
import { reducePayments } from "./Types";

export const path = "/payments/scheduled";

export default function ScheduledPayments(_props: unknown): JSX.Element {
  const [sortNetAsc, setSortNetAsc] = useState<"asc" | "desc">("asc");
  const [filterDirection, setDirectionfilter] = useState<
    "credit" | "debit" | "all"
  >("all");
  const scheduled = useGetScheduledPayments();

  if (scheduled.status !== "success") {
    return <div>Loading ({scheduled.status})</div>;
  }

  const mapped = orderBy(
    scheduled.data
      .map((s) => ({
        ...s,
        ledger: s.ledger[1] / s.ledger[2],
        paymentsSum: s.payments.reduce(reducePayments, 0),
      }))
      .map((s) => ({ ...s, netSum: s.ledger + s.paymentsSum })),
    ["netSum"],
    [sortNetAsc]
  ).filter((s) => {
    if (filterDirection === "all") {
      return true;
    }

    if (filterDirection === "credit") {
      if (s.netSum >= 0.01) {
        return true;
      } else {
        return false;
      }
    }

    if (filterDirection === "debit") {
      if (s.netSum <= -0.01) {
        return true;
      } else {
        return false;
      }
    }

    return true;
  });

  return (
    <div className="m-2">
      <div className="flex flex-row items-center my-4">
        <select
          onChange={(e) => {
            const val = e.target.value as "credit" | "debit" | "all";
            setDirectionfilter(val);
          }}
          value={filterDirection}
          className="block w-52 pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
        >
          <option value="all">All</option>
          <option value="credit">Credits Scheduled</option>
          <option value="debit">Debits scheduled</option>
        </select>

        <div className="ml-4">
          Total:{" "}
          {mapped
            .reduce((accum, s) => accum + s.netSum, 0)
            .toLocaleString("en-US", { style: "currency", currency: "USD" })}
        </div>

        <div className="ml-4">Count: {mapped.length}</div>
      </div>
      <div className="flex flex-col">
        <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
          <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
            <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <Th>User</Th>
                    <Th
                      onClick={() =>
                        setSortNetAsc(sortNetAsc === "asc" ? "desc" : "asc")
                      }
                    >
                      <span className="text-indigo-500 underline">
                        Scheduled payment ({sortNetAsc})
                      </span>
                    </Th>
                    <Th>Ledger</Th>
                    <Th>Pending payments</Th>
                  </tr>
                </thead>
                <tbody>
                  {mapped.map((scheduled, entryIdx) => (
                    <tr
                      key={scheduled.userid}
                      className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                    >
                      <Td>
                        <UserLink userid={scheduled.userid} />
                      </Td>
                      <Td>
                        {scheduled.netSum.toLocaleString("en-US", {
                          style: "currency",
                          currency: "USD",
                        })}{" "}
                        ({scheduled.netSum > 0 ? "Credit To" : "Debit from"})
                      </Td>
                      <Td>
                        {scheduled.ledger.toLocaleString("en-US", {
                          style: "currency",
                          currency: "USD",
                        })}
                      </Td>
                      <Td>
                        {scheduled.paymentsSum.toLocaleString("en-US", {
                          style: "currency",
                          currency: "USD",
                        })}
                      </Td>
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

interface Clickable {
  onClick?: MouseEventHandler<HTMLTableCellElement>;
}

const Th = (props: PropsWithChildren<Clickable>) => (
  <th
    scope="col"
    className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
    onClick={props.onClick}
  >
    {props.children}
  </th>
);

const Td = (props: PropsWithChildren<Clickable>) => (
  <td
    className="px-6 py-4 whitespace-nowrap text-sm text-gray-500"
    onClick={props.onClick}
  >
    {props.children}
  </td>
);
