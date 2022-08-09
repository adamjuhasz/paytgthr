/* eslint-disable @typescript-eslint/no-unsafe-member-access */

import { useState } from "react";
import { Link } from "react-router-dom";
import useAddLedgerEntry from "../User/UseAddLedgerEntry";

export const path = "/ledger/test/stress";

export default function StressTest(_p: unknown): JSX.Element {
  const [from, setFrom] = useState("00000000-0000-0000-0000-000000000000");
  const [to, setTo] = useState("00000000-0000-0000-0000-000000000000");
  const mutate = useAddLedgerEntry("00000000-0000-0000-0000-000000000000");

  return (
    <div>
      <form
        onSubmit={async (event) => {
          event.preventDefault();
          const test = Array(100)
            .fill(null)
            .map(() => mutate.mutateAsync({ from, to, amount: "0.01" }));

          console.log(test);

          await Promise.all(test);

          alert("Test Done");
        }}
      >
        <div>FROM:</div>
        <input
          name="from"
          type="text"
          value={from}
          onChange={(e) => setFrom(e.target.value)}
        />
        <div>To:</div>
        <input
          name="to"
          type="text"
          value={to}
          onChange={(e) => setTo(e.target.value)}
        />
        <div>
          <button
            className="mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-base font-medium text-gray-700 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:text-sm"
            type="submit"
          >
            Run test
          </button>
        </div>
        <div>
          <Link
            to={`/ledger/journals/${from}`}
            className="text-indigo-500 underline"
          >
            See from journal
          </Link>
        </div>
        <div>
          <Link
            to={`/ledger/journals/${to}`}
            className="text-indigo-500 underline"
          >
            See tp journal
          </Link>
        </div>
      </form>
    </div>
  );
}
