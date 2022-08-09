import { useParams } from "react-router-dom";
import { useMutation, useQuery } from "react-query";
import axios from "axios";
import { take } from "lodash";

import { serverlocation } from "../Backend/Server";
import TripleStats from "./TripleStats";

export const path = "/reports/:interval/user/:uid";

type PurchaseDescription = string;
type RewardName = string;
type UserFirstName = string;
type DateTimeUTC = string;

interface Response {
  globalMean: number;
  globalTopRewards: RewardName[];
  purchases: [PurchaseDescription, number, DateTimeUTC, RewardName][];
  rewardPotentialPercentEarned: number;
  rewardPotentialSum: number;
  rewardPotentialSumGrouped: [RewardName, number][];
  rewardSum: number;
  rewardSumGrouped: [RewardName, number][];
  riskCurrentLimit: number;
  riskCurrentScore: number;
  riskDollarToUp: number;
  riskNextLimit: number;
  spentGroupedPurchaser: [UserFirstName, number][];
  spentGroupedReward: [RewardName, number][];
  spentSum: number;
  spentSumGroupedReward: [RewardName, number][];
}

export default function UserWeeklyReport(): JSX.Element {
  const { uid, interval } = useParams();
  const realPath = path
    .replace(":uid", uid || "")
    .replace(":interval", interval || "");

  const result = useQuery(
    ["reports", "weekly", "user", uid],
    async (): Promise<Response> => {
      const res = await axios.get<Response>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    }
  );

  const mutation = useMutation(
    () => axios.post(`${serverlocation}${realPath}`, {}),
    {
      onSettled: () => {
        setTimeout(() => {
          mutation.reset();
        }, 3000);
      },
    }
  );

  let color = "indigo";
  switch (mutation.status) {
    case "loading":
      color = "yellow";
      break;
    case "success":
      color = "green";
      break;
    case "error":
      color = "red";
      break;
  }

  const numToDollar = (num: number): string =>
    num.toLocaleString("en-US", {
      style: "currency",
      currency: "USD",
    });

  if (result.status === "success") {
    return (
      <div className="px-2 py-2 max-w-2xl">
        <button
          type="button"
          className={`inline-flex items-center px-6 py-3 border border-transparent text-base font-medium rounded-md text-${color}-700 bg-${color}-100 hover:bg-${color}-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-${color}-500`}
          onClick={() => mutation.mutate()}
        >
          Send report to user
        </button>
        <div className="flex">
          <div className="w-6/12 my-2">
            <h3>Your top categories</h3>
            <ul className=" ">
              {take(result.data.spentGroupedReward, 5).map(
                (reward, rewardIndex) => (
                  <li key={reward[0]} className="py-4 flex">
                    <div className="h-10 w-10 rounded-full flex justify-center items-center bg-indigo-500 text-white ">
                      {rewardIndex + 1}
                    </div>
                    <div className="ml-3 ">
                      <p className="text-sm font-medium text-gray-900">
                        {reward[0] || "Everything else"}
                      </p>
                      <p className="text-sm text-gray-500">
                        {numToDollar(reward[1])}
                      </p>
                    </div>
                  </li>
                )
              )}
            </ul>
          </div>
          <div className="w-6/12">
            <h3>Global top categories</h3>
            <ul className="">
              {result.data.globalTopRewards.map((reward, rewardIndex) => (
                <li key={reward} className="py-4 flex">
                  <div className="h-10 w-10 rounded-full flex justify-center items-center bg-indigo-500 text-white ">
                    {rewardIndex + 1}
                  </div>
                  <div className="ml-3 flex items-center">
                    <p className="text-sm font-medium text-gray-900">
                      {reward}
                    </p>
                  </div>
                </li>
              ))}
            </ul>
          </div>
        </div>
        <TripleStats
          stats={[
            {
              name: "Total Spent",
              stat: numToDollar(result.data.spentSum),
            },
            {
              name: "Gobal Average Group Spend",
              stat: numToDollar(result.data.globalMean),
            },
          ]}
        />
        <TripleStats
          stats={[
            {
              name: "Total Earned",
              stat: numToDollar(result.data.rewardSum),
            },
            {
              name: "Percent reward earned",
              stat: `${result.data.rewardPotentialPercentEarned.toFixed(0)}%`,
            },
            {
              name: "Could have Earned",
              stat: numToDollar(result.data.rewardPotentialSum),
            },
          ]}
        />
        <div className="w-full flex flex-row justify-between items-center my-2">
          <div>{numToDollar(result.data.rewardSum)}</div>
          <div className="rounded-full flex-1 h-10 overflow-hidden shadow-lg mx-2">
            <div
              className="bg-rose-500  h-10 rounded-full flex justify-center items-center"
              style={{
                width: `${result.data.rewardPotentialPercentEarned.toFixed(
                  0
                )}%`,
              }}
            >
              <div className="text-white">
                {result.data.rewardPotentialPercentEarned.toFixed(0)}%
              </div>
            </div>
          </div>
          <div>{numToDollar(result.data.rewardPotentialSum)}</div>
        </div>
        <h2>Spent by reward</h2>
        <TripleStats
          stats={result.data.spentGroupedReward.map((r) => ({
            name: r[0] || "Everything else",
            stat: numToDollar(r[1]),
          }))}
        />
        <h2>Earned by reward</h2>
        <TripleStats
          stats={result.data.rewardSumGrouped.map((r) => ({
            name: r[0] || "Everything else",
            stat: numToDollar(r[1]),
          }))}
        />
        <h2>Spent by each person</h2>
        <TripleStats
          count={2}
          stats={result.data.spentGroupedPurchaser.map((r) => ({
            name: r[0],
            stat: numToDollar(r[1]),
          }))}
        />
        <div className="bg-white shadow overflow-hidden sm:rounded-md mt-2">
          <ul className="divide-y divide-gray-200">
            {result.data.purchases.map((purchase) => (
              <li key={purchase[2]}>
                <div className="px-4 py-4 sm:px-6">
                  <div className="flex items-center justify-between">
                    <p className="text-sm font-medium text-indigo-600 truncate">
                      {purchase[0]}
                    </p>
                    <span className="inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-gray-100 text-gray-800">
                      {purchase[3]}
                    </span>
                  </div>
                  <div className="mt-0 flex justify-between">
                    <div className="flex">
                      <p className="flex items-center text-sm text-gray-500">
                        {numToDollar(purchase[1])}
                      </p>
                    </div>
                  </div>
                </div>
              </li>
            ))}
          </ul>
        </div>
      </div>
    );
  }

  return <></>;
}
