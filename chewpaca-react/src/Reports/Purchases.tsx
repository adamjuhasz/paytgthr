import { useMemo, useState } from "react";
import { chain, defaultTo } from "lodash";
import {
  differenceInCalendarMonths,
  format,
  getDate,
  getDaysInMonth,
  startOfMonth,
  subMonths,
} from "date-fns";
import { CSVLink } from "react-csv";

import useGetPurchases from "./UseGetPurchases";
import useGetGroups from "../Group/UseGetGroups";
import useGetUsers from "../Users/UseGetUsers";

export const path = "/reports/purchases";

export default function ChurnReport(): JSX.Element {
  const [monthsToLoad] = useState<number | undefined>(36);
  const [filterActiveUsers, setFilterActiveUser] = useState(true);
  const [filterActiveGroup, setFilterActiveGroup] = useState(false);
  const [extrapolateCurrMonth, setExtrapolate] = useState(true);

  const now = useMemo(() => new Date(), []);
  const monthsAgo = startOfMonth(subMonths(now, defaultTo(monthsToLoad, 24)));

  const purchases = useGetPurchases(monthsAgo, [
    "TrxCompleted",
    "TrxAuthorized",
    "TrxPending",
  ]);
  const purchasesToScan = chain(purchases.data)
    .defaultTo([])
    .filter((p) => p.groupid !== null)
    .filter((p) => p.state.kind !== "trxdeclined")
    .filter((p) => p.displayamount[1] / p.displayamount[2] !== 0)
    .value();

  const groupList = chain(purchasesToScan)
    .map((p) => (p.groupid === null ? "!!!@@@!!!" : p.groupid[0]))
    .uniq()
    .value();

  console.log(purchases.data?.length, purchasesToScan.length, groupList.length);

  const groups = useGetGroups(purchases.status === "success", groupList);
  const usersToGet = chain(groups.data)
    .defaultTo([])
    .flatMap((g) => g.members.map((m) => m.user))
    .value();
  const users = useGetUsers(groups.status === "success", usersToGet);

  const groupsById = chain(groups.data)
    .defaultTo([])
    .groupBy((g) => g.id)
    .value();
  const usersById = chain(users.data)
    .defaultTo([])
    .groupBy((u) => u.userid)
    .mapValues((us) => us[0])
    .value();

  console.log(
    groupList.length,
    groups.data?.length,
    chain(groupsById).keys().value().length
  );

  const purchasesByMonthGroup = chain(purchasesToScan)
    .filter((p) => {
      if (groups.status !== "success" || users.status !== "success") {
        return true;
      }

      if (p.groupid === null) {
        throw new Error("groupid is null");
      }

      const groupInfo = groupsById[p.groupid[0]]?.[0];
      if (groupInfo === undefined) {
        throw new Error("groupInfo is undefined");
      }

      if (filterActiveGroup === true) {
        if (groupInfo.status !== "groupactive") {
          return false;
        }
      }

      if (filterActiveUsers === true) {
        const members = groupInfo.members.map((m) => m.user);
        if (members.length !== 2) {
          throw new Error("members is not 2");
        }

        const userList = members
          .map((u) => usersById[u])
          .filter((u) => u !== undefined)
          .filter((u) => u.userstate.tag === "UserActive");
        // if (userList.length !== 2) {
        //   console.error("userList", userList, p.groupid);
        // }

        if (userList.length !== 2) {
          return false;
        }
      }

      return true;
    })
    .groupBy((p) => startOfMonth(new Date(p.purchasedat)).toISOString())
    .mapValues((ps, month) => {
      const groupedById = chain(ps).groupBy((p) =>
        p.groupid === null ? "!!!!!!" : p.groupid[0]
      );

      let factor = getDaysInMonth(new Date()) / getDate(new Date());
      factor =
        extrapolateCurrMonth &&
        differenceInCalendarMonths(new Date(), new Date(month)) === 0
          ? factor
          : 1;
      return groupedById
        .mapValues((p) =>
          p.reduce(
            (accum, curr) =>
              accum + (curr.displayamount[1] / curr.displayamount[2]) * factor,
            0
          )
        )
        .value();
    })
    .value();

  if (
    purchases.status !== "success" &&
    groups.status !== "success" &&
    users.status !== "success"
  ) {
    return (
      <>
        <div>Purchase status: {purchases.status}</div>
        <div>Groups Status: {groups.status}</div>
        <div>Users Status: {users.status}</div>
      </>
    );
  }

  const stats = chain(purchasesByMonthGroup)
    .map((val, key) => ({ month: key, groups: val }))
    .value()
    .map((m) => ({
      month: m.month,
      count: chain(m.groups).keys().value().length,
      sum: chain(m.groups)
        .reduce((accum, g) => accum + g)
        .value(),
      mean: chain(m.groups)
        .map((g) => g)
        .mean()
        .value(),
      median: quantile(
        chain(m.groups)
          .map((g) => g)
          .value(),
        0.5
      ),
      "75": quantile(
        chain(m.groups)
          .map((g) => g)
          .value(),
        0.75
      ),
      "90": quantile(
        chain(m.groups)
          .map((g) => g)
          .value(),
        0.9
      ),
      top: chain(m.groups)
        .map((g) => g)
        .orderBy([(g) => g], ["desc"])
        .value()[0],
    }));

  return (
    <div className="m-2">
      <fieldset className="space-y-5">
        <div className="relative flex items-start">
          <div className="flex items-center h-5">
            <input
              id="filterActiveUsers"
              name="filterActiveUsers"
              type="checkbox"
              className="focus:ring-indigo-500 h-4 w-4 text-indigo-600 border-gray-300 rounded"
              checked={filterActiveUsers}
              onChange={(event) => setFilterActiveUser(event.target.checked)}
            />
          </div>
          <div className="ml-3 text-sm">
            <label
              htmlFor="filterActiveUsers"
              className="font-medium text-gray-700"
            >
              Use active users only
            </label>
            <p id="filterActiveUsers-description" className="text-gray-500">
              Only include active users
            </p>
          </div>
        </div>

        <div className="relative flex items-start">
          <div className="flex items-center h-5">
            <input
              id="filterActiveGroup"
              name="filterActiveGroup"
              type="checkbox"
              className="focus:ring-indigo-500 h-4 w-4 text-indigo-600 border-gray-300 rounded"
              checked={filterActiveGroup}
              onChange={(event) => setFilterActiveGroup(event.target.checked)}
            />
          </div>
          <div className="ml-3 text-sm">
            <label
              htmlFor="filterActiveGroup"
              className="font-medium text-gray-700"
            >
              Use active groups only
            </label>
            <p id="filterActiveGroup-description" className="text-gray-500">
              Only include active groups
            </p>
          </div>
        </div>

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

      <div>
        purchases analyzed: {purchasesToScan.length},{" "}
        {defaultTo(purchases.data, []).length - purchasesToScan.length}{" "}
        discarded
      </div>
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
                        Groups
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Total
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Mean
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Median (50%)
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Quartile (75%)
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Quartile (90%)
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Top
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
                          {m.count}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.sum.toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.mean.toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.median.toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m["75"].toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m["90"].toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.top.toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
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

const quantile = (arr: number[], q: number) => {
  const sorted = arr.sort((a, b) => a - b);
  const pos = (sorted.length - 1) * q;
  const base = Math.floor(pos);
  const rest = pos - base;
  if (sorted[base + 1] !== undefined) {
    return sorted[base] + rest * (sorted[base + 1] - sorted[base]);
  } else {
    return sorted[base];
  }
};
