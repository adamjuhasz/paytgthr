import { useMemo, useState } from "react";
import { chain, defaultTo, difference, keys } from "lodash";
import {
  addMonths,
  differenceInCalendarMonths,
  format,
  startOfMonth,
  subMonths,
} from "date-fns";
import { Axis, LineSeries, XYChart } from "@visx/xychart";
import * as Curves from "@visx/curve";
import { CSVLink } from "react-csv";

import useGetPurchases from "./UseGetPurchases";
import useGetGroups from "../Group/UseGetGroups";
import useGetUsers from "../Users/UseGetUsers";

export const path = "/reports/churn";

export default function ChurnReport(): JSX.Element {
  console.log("render");
  const [monthsToLoad] = useState<number | undefined>(36);
  const [monthsToShow, setMonthsToShow] = useState<number | undefined>(25);
  const [churnMonths, setChurnMonths] = useState<number | undefined>(1);
  const [filterClosedUsers, setFilterClosed] = useState(true);
  const [filterIncompleteMonhts, setFilterIncomplete] = useState(true);
  const [smooth, setSmooth] = useState(true);

  const now = useMemo(() => new Date(), []);
  const monthsAgo = startOfMonth(subMonths(now, defaultTo(monthsToLoad, 36)));

  const purchases = useGetPurchases(monthsAgo, [
    "TrxCompleted",
    "TrxPending",
    "TrxAuthorized",
  ]);
  const purchasesToScan = chain(purchases.data)
    .defaultTo([])
    .filter((p) => p.groupid !== null)
    .filter((p) => p.state.kind !== "trxdeclined")
    .filter((p) => p.displayamount[1] / p.displayamount[2] !== 0)
    .value();
  const purchasesByGroup = chain(purchasesToScan)
    .groupBy((p) => (p.groupid === null ? "!!!@@@!!!" : p.groupid[0]))
    .value();
  const groups = useGetGroups(
    purchases.status === "success",
    keys(purchasesByGroup)
  );
  const usersToGet = chain(groups.data)
    .defaultTo([])
    .flatMap((g) => g.members.map((m) => m.user))
    .value();
  const users = useGetUsers(groups.status === "success", usersToGet);
  const usersById = chain(users.data)
    .defaultTo([])
    .groupBy((u) => u.userid)
    .value();

  const groupsToUse = chain(groups.data)
    .defaultTo([])
    .filter((g) => {
      if (users.status !== "success") {
        return true;
      }

      if (filterClosedUsers === false) {
        return true;
      }

      const members = chain(g.members)
        .flatMap((m) => usersById[m.user])
        .filter((u) => u.userstate.tag === "UserActive")
        .value();
      if (members.length === 2) {
        return true;
      } else {
        return false;
      }
    })
    .value();

  const monthlyChurn = useMemo(() => {
    const groupsByCreated = chain(groupsToUse)
      .defaultTo([])
      .groupBy((g) => startOfMonth(new Date(g.createdat)).toISOString())
      .value();

    const monthsToScan = chain(groupsByCreated)
      .keys()
      .orderBy((x) => x, ["desc"])
      .filter((m) => {
        if (filterIncompleteMonhts === false) {
          return differenceInCalendarMonths(now, new Date(m)) !== 0;
        }

        return (
          differenceInCalendarMonths(now, new Date(m)) >
          defaultTo(churnMonths, 1)
        );
      })
      .filter((m) => {
        return (
          differenceInCalendarMonths(now, new Date(m)) <
          defaultTo(monthsToShow, 36)
        );
      })
      .value();

    return chain(monthsToScan)
      .map((month) => {
        const groupsToCount = groupsByCreated[month].map((g) => g.id);

        const checkMonth = addMonths(
          new Date(month),
          defaultTo(churnMonths, 1)
        ).toISOString();
        const startNumber = groupsToCount.length;

        const groupsInCheckMonth = groupsToCount.filter((groupId) => {
          const purchases = purchasesByGroup[groupId];
          const purchasesByMonth = chain(purchases)
            .groupBy((p) => startOfMonth(new Date(p.purchasedat)).toISOString())
            .mapValues((ps) => ps.length)
            .value();
          const purchasesInCheckMonth = chain(purchasesByMonth[checkMonth])
            .defaultTo(0)
            .value();
          return purchasesInCheckMonth !== 0;
        });
        const endNumber = groupsInCheckMonth.length;

        return {
          month: month,
          start: startNumber,
          end: endNumber,
          churn: (startNumber - endNumber) / startNumber,
          retention: endNumber / startNumber,
          groupsStart: groupsToCount,
          groupsEnd: groupsInCheckMonth,
        };
      })
      .value();
  }, [
    churnMonths,
    filterIncompleteMonhts,
    monthsToShow,
    groupsToUse,
    now,
    purchasesByGroup,
  ]);

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

  return (
    <div className="m-2">
      <div>
        <label
          htmlFor="price"
          className="block text-sm font-medium text-gray-700"
        >
          Churn to calculate
        </label>
        <div className="mt-1 relative rounded-md shadow-sm">
          <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
            <span className="text-gray-500 sm:text-sm"></span>
          </div>
          <input
            type="text"
            name="price"
            id="price"
            className="focus:ring-indigo-500 focus:border-indigo-500 block w-full pl-7 pr-12 sm:text-sm border-gray-300 rounded-md"
            placeholder="1"
            aria-describedby="price-currency"
            value={churnMonths === undefined ? "" : churnMonths.toFixed(0)}
            onChange={(t) =>
              t.target.value === ""
                ? setChurnMonths(undefined)
                : setChurnMonths(parseInt(t.target.value, 10))
            }
          />
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            <span className="text-gray-500 sm:text-sm" id="price-currency">
              Month churn
            </span>
          </div>
        </div>

        <label
          htmlFor="showmonths"
          className="block text-sm font-medium text-gray-700"
        >
          Months to show
        </label>
        <div className="mt-1 relative rounded-md shadow-sm">
          <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
            <span className="text-gray-500 sm:text-sm"></span>
          </div>
          <input
            type="text"
            name="showmonths"
            id="showmonths"
            className="focus:ring-indigo-500 focus:border-indigo-500 block w-full pl-7 pr-12 sm:text-sm border-gray-300 rounded-md"
            placeholder="1"
            aria-describedby="price-currency"
            value={monthsToShow === undefined ? "" : monthsToShow.toFixed(0)}
            onChange={(t) =>
              t.target.value === ""
                ? setMonthsToShow(undefined)
                : setMonthsToShow(parseInt(t.target.value, 10))
            }
          />
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            <span className="text-gray-500 sm:text-sm" id="price-currency">
              months
            </span>
          </div>
        </div>

        <fieldset className="space-y-5">
          <div className="relative flex items-start">
            <div className="flex items-center h-5">
              <input
                id="closed"
                name="closed"
                type="checkbox"
                className="focus:ring-indigo-500 h-4 w-4 text-indigo-600 border-gray-300 rounded"
                checked={filterClosedUsers}
                onChange={(event) => setFilterClosed(event.target.checked)}
              />
            </div>
            <div className="ml-3 text-sm">
              <label htmlFor="closed" className="font-medium text-gray-700">
                Filter closed users out
              </label>
              <p id="closed-description" className="text-gray-500">
                Do not include closed users in calculations
              </p>
            </div>
          </div>

          <div className="relative flex items-start">
            <div className="flex items-center h-5">
              <input
                id="incomplete"
                name="incomplete"
                type="checkbox"
                className="focus:ring-indigo-500 h-4 w-4 text-indigo-600 border-gray-300 rounded"
                checked={filterIncompleteMonhts}
                onChange={(event) => setFilterIncomplete(event.target.checked)}
              />
            </div>
            <div className="ml-3 text-sm">
              <label htmlFor="incomplete" className="font-medium text-gray-700">
                Filter incomplete moths out
              </label>
              <p id="incomplete-description" className="text-gray-500">
                Do not include incomplete months
              </p>
            </div>
          </div>

          <div className="relative flex items-start">
            <div className="flex items-center h-5">
              <input
                id="smooth"
                name="smooth"
                type="checkbox"
                className="focus:ring-indigo-500 h-4 w-4 text-indigo-600 border-gray-300 rounded"
                checked={smooth}
                onChange={(event) => setSmooth(event.target.checked)}
              />
            </div>
            <div className="ml-3 text-sm">
              <label htmlFor="smooth" className="font-medium text-gray-700">
                Smooth graph
              </label>
              <p id="smooth-description" className="text-gray-500">
                Make it curvy
              </p>
            </div>
          </div>
        </fieldset>
      </div>

      <div>
        purchases analyzed: {purchasesToScan.length},{" "}
        {defaultTo(purchases.data, []).length - purchasesToScan.length}{" "}
        discarded
      </div>
      <div>Groups involved: {groupsToUse.length}</div>
      <CSVLink
        data={monthlyChurn}
        className="text-rose-500 underline"
        filename="chewpaca-data.csv"
      >
        Download CSV
      </CSVLink>
      <div>
        <a
          className="text-rose-500 underline"
          href={`data:text/json;charset=utf-8,${encodeURIComponent(
            JSON.stringify(monthlyChurn)
          )}`}
          download="chewpaca-data.json"
        >
          Download JSON
        </a>
      </div>
      <Charting
        data={defaultTo(monthlyChurn, []).filter((m) => m.month !== undefined)}
        smooth={smooth}
      />
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
                        Churn
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Retention
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Groups that at least 1 puchase in lifetime
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        made a purchase {churnMonths} month(s) later too
                      </th>
                      <th
                        scope="col"
                        className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                      >
                        Churned groups
                      </th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {monthlyChurn.map((m) => (
                      <tr key={m.month}>
                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                          {format(new Date(m.month), "MMMM yyyy")}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {(m.churn * 100).toFixed(0)}%
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {(m.retention * 100).toFixed(0)}%
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.start}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {m.end}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {difference(m.groupsStart, m.groupsEnd).map((g) => (
                            <>
                              <a
                                className="text-rose-500 underline"
                                href={`/group/${g}`}
                              >
                                {g}
                              </a>
                              <br />
                            </>
                          ))}
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

interface ChartProps {
  data: { month: string; retention: number }[];
  smooth: boolean;
}

const Charting = (props: ChartProps) => (
  <XYChart
    height={270}
    margin={{ left: 60, top: 35, bottom: 38, right: 27 }}
    xScale={{ type: "time" }}
    yScale={{ type: "linear", domain: [0, 100] }}
  >
    <Axis
      hideAxisLine
      hideTicks
      orientation="bottom"
      tickLabelProps={() => ({ dy: 10 })}
      left={30}
      numTicks={4}
    />
    <Axis
      hideAxisLine
      hideTicks
      orientation="left"
      numTicks={5}
      tickLabelProps={() => ({ dx: -10 })}
    />
    <LineSeries
      stroke="#10b981"
      dataKey="retention"
      data={props.data}
      xAccessor={(d) => (d === undefined ? null : new Date(d.month))}
      yAccessor={(d) => (d === undefined ? 0 : Math.round(d.retention * 100))}
      curve={props.smooth ? Curves.curveMonotoneX : Curves.curveLinear}
    />
  </XYChart>
);
