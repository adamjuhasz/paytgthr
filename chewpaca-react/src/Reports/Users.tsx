import { useState } from "react";
import { chain } from "lodash";
import {
  differenceInCalendarMonths,
  format,
  getDate,
  getDaysInMonth,
  startOfMonth,
} from "date-fns";
import { CSVLink } from "react-csv";

import useGetUsers from "../Users/UseGetUsers";

export const path = "/reports/users";

export default function ChurnReport(): JSX.Element {
  const [extrapolateCurrMonth, setExtrapolate] = useState(true);

  const users = useGetUsers();

  const extrapolate = (month: string, val: number) =>
    Math.round(
      val *
        (extrapolateCurrMonth &&
        differenceInCalendarMonths(new Date(), new Date(month)) === 0
          ? getDaysInMonth(new Date()) / getDate(new Date())
          : 1)
    );

  const stats = chain(users.data)
    .defaultTo([])
    .groupBy((u) => startOfMonth(new Date(u.createdon)).toISOString())
    .map((users, month) => ({
      month: month,
      signedUp: extrapolate(month, users.length),
      active: extrapolate(
        month,
        users.filter((u) => u.userstate.tag === "UserActive").length
      ),
      verifiedBank: extrapolate(
        month,
        users.filter(
          (u) => u.userstate.tag === "UserActive" && u.bankverified === true
        ).length
      ),
      closed: extrapolate(
        month,
        users.filter((u) => u.userstate.tag === "UserClosed").length
      ),
      closedByUser: extrapolate(
        month,
        users.filter(
          (u) =>
            u.userstate.tag === "UserClosed" &&
            u.userstate.contents === "ClosedByUser"
        ).length
      ),
      closedFraudy: extrapolate(
        month,
        users.filter(
          (u) =>
            u.userstate.tag === "UserClosed" &&
            (u.userstate.contents === "FraudyUser" ||
              u.userstate.contents === "OverdueBalance")
        ).length
      ),
    }))
    .value();

  if (users.status !== "success") {
    return (
      <>
        <div>Users Status: {users.status}</div>
      </>
    );
  }

  return (
    <div className="m-2">
      <fieldset className="space-y-5">
        <div className="relative flex items-start">
          <div className="flex items-center h-5">
            <input
              id="extrapolateCurrMonth"
              name="extrapolateCurrMonth"
              type="checkbox"
              className="focus:ring-indigo-500 h-4 w-4 text-indigo-600 border-gray-300 rounded"
              checked={extrapolateCurrMonth}
              onChange={(event) => setExtrapolate(event.target.checked)}
            />
          </div>
          <div className="ml-3 text-sm">
            <label
              htmlFor="extrapolateCurrMonth"
              className="font-medium text-gray-700"
            >
              Extrapolate current month
            </label>
            <p className="text-gray-500">Linear</p>
          </div>
        </div>
      </fieldset>

      <CSVLink
        data={stats}
        className="text-rose-500 underline"
        filename="chewpaca-data.csv"
      >
        Download CSV
      </CSVLink>
      <div>
        <a
          className="text-rose-500 underline"
          href={`data:text/json;charset=utf-8,${encodeURIComponent(
            JSON.stringify(stats)
          )}`}
          download="chewpaca-data.json"
        >
          Download JSON
        </a>
      </div>
      <div>
        <div className="flex flex-col">
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
                        Month
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Signed up
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Active
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Verified bank
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Closed by user
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Fraudy
                      </th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {stats.map((m) => (
                      <tr key={m.month}>
                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                          {format(new Date(m.month), "MMMM yyyy")}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.signedUp}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.active}{" "}
                          <span className="text-gray-300">
                            ({((m.active / m.signedUp) * 100).toFixed(0)}% of
                            signed up)
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.verifiedBank}{" "}
                          <span className="text-gray-300">
                            ({((m.verifiedBank / m.active) * 100).toFixed(0)}%
                            of active)
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.closedByUser}{" "}
                          <span className="text-gray-300">
                            ({((m.closedByUser / m.signedUp) * 100).toFixed(0)}%
                            of signed up)
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.closedFraudy}{" "}
                          <span className="text-gray-300">
                            ({((m.closedFraudy / m.signedUp) * 100).toFixed(0)}%
                            of signed up)
                          </span>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
