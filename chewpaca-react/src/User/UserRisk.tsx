import { PropsWithChildren, useEffect, useState } from "react";
import { defaultTo } from "lodash";

import { useGetRiskScores } from "./UseGetRiskScores";
import { TimeInterval, filterDate } from "../Shared/IntervalFilter";
import ShowTrustScore from "../Risk/ShowTrustScore";
import ShowDollarToNext from "../Risk/ShowDollarToNext";

interface Props {
  user: string;
}

export default function UserRisk(props: Props): JSX.Element {
  const scores = useGetRiskScores(props.user);
  const [interval, setIntervalVal] = useState<TimeInterval>("30 days");
  const filtered = defaultTo(scores.data, []).filter((s) =>
    filterDate(s, "createdat", interval)
  );
  useEffect(() => {
    if (scores.status === "success" && filtered.length === 0) {
      setIntervalVal("All");
    }
  }, [scores.status, filtered, setIntervalVal]);

  if (scores.status === "success") {
    return (
      <>
        <form
          method="post"
          action={`/user/${props.user}/risk/adjustment`}
          className="mb-4"
        >
          <div className="bg-white overflow-hidden shadow rounded-lg">
            <div className="px-4 py-5 sm:p-6">
              <label
                htmlFor="riskadjustment"
                className="block text-sm font-medium leading-5 text-gray-700"
              >
                Manual Adjustment to (0 - 100)
              </label>
              <div className="mt-1 relative rounded-md shadow-sm">
                <input
                  name="trustscore"
                  id="riskadjustment"
                  placeholder="40"
                  type="text"
                  autoComplete="off"
                  className="form-input block w-full sm:text-sm sm:leading-5"
                />
              </div>
              <div className="mt-2">
                <span className="inline-flex rounded-md shadow-sm">
                  <button
                    type="submit"
                    className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                  >
                    Change Score
                  </button>
                </span>
              </div>
            </div>
          </div>
        </form>
        <div className="flex flex-row items-center mt-1 mb-4 justify-between ">
          <select
            onChange={(e) => {
              const val = e.target.value as TimeInterval;
              setIntervalVal(val);
            }}
            className="block w-52 pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
          >
            <option value="30 days" selected={interval === "30 days"}>
              30 days of history
            </option>
            <option value="60 days" selected={interval === "60 days"}>
              60 days of history
            </option>
            <option value="90 days" selected={interval === "90 days"}>
              90 days of history
            </option>
            <option value="All" selected={interval === "All"}>
              All history
            </option>
          </select>
          <div></div>
        </div>
        <div className="flex flex-col max-w-full">
          <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
            <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
              <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <Th>Rev</Th>
                      <Th>Fact</Th>
                      <Th>Reason</Th>
                      <Th>Change</Th>
                      <Th>Limit</Th>
                      <Th>Spend to next</Th>
                    </tr>
                  </thead>
                  <tbody>
                    {filtered.map((score, entryIdx) => (
                      <tr
                        key={score.rev}
                        className={
                          entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"
                        }
                      >
                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                          <span className="text-gray-400">({score.rev})</span>{" "}
                          {new Date(score.createdat).toLocaleString()}
                        </td>
                        <Td>{score.trustscore}</Td>
                        <Td>{score.fact.kind}</Td>
                        <Td>
                          {score.change >= 0 ? (
                            <span>+{score.change.toFixed(1)}</span>
                          ) : (
                            <span className="text-red-500">
                              {score.change.toFixed(2)}
                            </span>
                          )}
                        </Td>
                        <Td>
                          <ShowTrustScore score={score.trustscore} />
                        </Td>
                        <Td>
                          <ShowDollarToNext score={score.trustscore} />
                        </Td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </>
    );
  }

  return <div>Loading scores</div>;
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
