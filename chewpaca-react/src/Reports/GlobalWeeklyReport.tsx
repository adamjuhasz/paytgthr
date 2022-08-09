import { meanBy, sortBy } from "lodash";

import TripleStats from "./TripleStats";

import useGetWeeklyGlobal, { path as getPath } from "./UseGetWeeklyGlobal";
export const path = getPath;

export default function GlobalWeeklyReport(): JSX.Element {
  const result = useGetWeeklyGlobal();

  if (result.status === "success") {
    const numGroups = 10;
    const convertedCurr = sortBy(
      result.data.sumPerGroup,
      (g) => g[1][1] / g[1][2]
    )
      .reverse()
      .slice(0, numGroups);

    return (
      <div className="px-4 pt-8">
        <h3 className="text-lg leading-6 font-medium text-gray-900">
          Last 7 days
        </h3>
        <TripleStats
          stats={[
            {
              name: "Total purchase attempts",
              stat: result.data.purchases.length.toString(),
            },
            {
              name: "Succesful purchases",
              stat: result.data.purchases
                .filter((t) => t.state.kind !== "trxdeclined")
                .length.toString(),
            },
            {
              name: "Succesful $",
              stat: result.data.purchases
                .filter((t) => t.state.kind !== "trxdeclined")
                .reduce(
                  (accum, t) => accum + t.displayamount[1] / t.displayamount[2],
                  0
                )
                .toLocaleString("en-US", {
                  style: "currency",
                  currency: "USD",
                }),
            },
          ]}
        />
        <TripleStats
          count={4}
          stats={[
            {
              name: "Total groups",
              stat: result.data.grouped.length.toString(),
            },
            {
              name: "Median group spend",
              stat: (
                result.data.totalMedian[1] / result.data.totalMedian[2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
            {
              name: "Mean group spend",
              stat: (
                result.data.totalMean[1] / result.data.totalMean[2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
            {
              name: `Top ${numGroups} Groups Spent on Avg`,
              stat: meanBy(
                convertedCurr,
                (g) => g[1][1] / g[1][2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
          ]}
        />
        <TripleStats
          count={5}
          color="rose"
          stats={[
            {
              name: "Group #1",
              stat: (
                convertedCurr[0][1][1] / convertedCurr[0][1][2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
            {
              name: "Group #2",
              stat: (
                convertedCurr[1][1][1] / convertedCurr[1][1][2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
            {
              name: "Group #3",
              stat: (
                convertedCurr[2][1][1] / convertedCurr[2][1][2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
            {
              name: "Group #4",
              stat: (
                convertedCurr[3][1][1] / convertedCurr[3][1][2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
            {
              name: "Group #5",
              stat: (
                convertedCurr[4][1][1] / convertedCurr[4][1][2]
              ).toLocaleString("en-US", {
                style: "currency",
                currency: "USD",
              }),
            },
          ]}
        />
        <table>
          {result.data.spentByReward.map((r) => (
            <tr key={r[0]}>
              <td>{r[0]}</td>
              <td>
                {(r[1][1] / r[1][2]).toLocaleString("en-US", {
                  style: "currency",
                  currency: "USD",
                })}
              </td>
            </tr>
          ))}
        </table>
      </div>
    );
  }

  return <></>;
}
