import { useEffect, useState } from "react";
import { defaultTo } from "lodash";

import useGetPurchases from "./UseGetPurchases";

import PurchaseList from "../Purchases/PurchaseTable";
import { TimeInterval, filterDate } from "../Shared/IntervalFilter";

interface Props {
  user: string;
}

export default function UserPurchases(props: Props): JSX.Element {
  const purchases = useGetPurchases(props.user);
  const [interval, setIntervalVal] = useState<TimeInterval>("30 days");
  const filtered = defaultTo(purchases.data, []).filter((p) =>
    filterDate(p, "purchasedat", interval)
  );

  useEffect(() => {
    if (purchases.status === "success" && filtered.length === 0) {
      setIntervalVal("All");
    }
  }, [purchases.status, filtered, setIntervalVal]);

  if (purchases.status === "success") {
    return (
      <>
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
        <PurchaseList purchases={filtered} thisUser={props.user} />
      </>
    );
  }

  return <div>Loading purchases...</div>;
}
