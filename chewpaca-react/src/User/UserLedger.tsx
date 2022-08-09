/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { FormEvent, useState } from "react";
import { RadioGroup } from "@headlessui/react";
import { CheckCircleIcon } from "@heroicons/react/solid";
import { useNavigate } from "react-router";
import { orderBy } from "lodash";

import useGetJournals from "./UseGetJournals";
import useGeFullLEdger from "./UseGetFullLedger";
import EntriesFeed from "../Ledger/EntriesFeed";
import JournalFeed from "../Ledger/JournalFeed";
import useChangeJournals from "./UseChangeJournals";
import { TimeInterval, filterDate } from "../Shared/IntervalFilter";
import useDoLedgerPayment from "./UseDoLedgerPayment";
import Spinner from "../Shared/Spinner";
import useAddLedgerEntry from "./UseAddLedgerEntry";

interface Props {
  user: string;
}

function classNames(...classes: string[]) {
  return classes.filter(Boolean).join(" ");
}

export default function UserLedger(props: Props): JSX.Element {
  const journals = useGetJournals(props.user);
  const [selected, setSelected] = useState<null | string>(null);
  const allEntries = useGeFullLEdger(props.user, selected === null);
  const [interval, setIntervalVal] = useState<TimeInterval>("30 days");

  const changeJournals = useChangeJournals(props.user);
  const doLedgerPayment = useDoLedgerPayment(props.user);
  const navigate = useNavigate();
  const addEntry = useAddLedgerEntry(props.user);

  let contents = (
    <>
      <form
        className="bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10"
        method="post"
        action="#"
        onSubmit={async (event: FormEvent<HTMLFormElement>) => {
          event.preventDefault();
          const elements = event.currentTarget.elements as any;
          const jtype = (elements.jtype as HTMLInputElement).value;
          const dwolla = (elements.dwolla as HTMLInputElement).value;
          await changeJournals.mutateAsync({
            journalType: jtype,
            dwollaId: dwolla,
          });
        }}
      >
        <div className="">
          <div className="">
            <label
              htmlFor="name"
              className="block text-sm font-medium text-gray-700"
            >
              Journal Type
            </label>
            <select
              name="jtype"
              className="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
            >
              <option value="PayTgthr">Pay Tgthr</option>
              <option value="StashTgthr">Stash Tgthr</option>
              <option value="SaveTgthr">Save Tgthr</option>
              <option value="FundingSource">Funding Source</option>
            </select>
            <label
              htmlFor="dwolla"
              className="mt-2 block text-sm font-medium text-gray-700"
            >
              Dwolla ID
            </label>
            <input
              name="dwolla"
              id="dwolla"
              placeholder="https://..."
              type="text"
              autoComplete="off"
              className="shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
            />
            <div className="mt-2">
              <button
                disabled={changeJournals.isLoading}
                type="submit"
                className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
              >
                {changeJournals.isLoading ? <Spinner /> : "New journal"}
              </button>
            </div>
          </div>
        </div>
      </form>
    </>
  );

  if (journals.status === "success") {
    contents = (
      <>
        {contents}
        <div className="mt-4">
          <form
            method="post"
            action="#"
            className="bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10"
            onSubmit={async (event: FormEvent<HTMLFormElement>) => {
              event.preventDefault();
              const elements = event.currentTarget.elements as any;
              const amount = (elements.amount as HTMLInputElement).value;
              const from = (elements.from as HTMLInputElement).value;
              const to = (elements.to as HTMLInputElement).value;
              await addEntry.mutateAsync({
                amount,
                from,
                to,
              });
            }}
          >
            <div className="">
              <div className="">
                <label
                  htmlFor="ledgerchange"
                  className="block text-sm font-medium text-gray-700"
                >
                  Manual Adjustment
                </label>
                <input
                  name="amount"
                  id="ledgerchange"
                  placeholder="1.12"
                  type="text"
                  autoComplete="off"
                  className="shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
                />
                <p className="mt-1 text-sm text-gray-500">
                  without dollar sign, 2 decimal places, +/- ex: (1.12) (-13.45)
                </p>
                <label
                  htmlFor="from"
                  className="mt-2 block text-sm font-medium text-gray-700"
                >
                  From Journal
                </label>
                <select
                  name="from"
                  id="from"
                  className="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
                >
                  <option>Pick one of these as FROM</option>
                  <option value="00000000-0000-0000-0000-000000000000">
                    Purchase disputes (Lithic) (6c101d7a)
                  </option>
                  <option value="00000000-0000-0000-0000-000000000000">
                    Pay Tgthr Rewards (df0dba90)
                  </option>
                  <option value="00000000-0000-0000-0000-000000000000">
                    Customer Research Rewards (82620720)
                  </option>
                  <option value="00000000-0000-0000-0000-000000000000">
                    Account Growth Rewards (bc1ef6e9)
                  </option>
                  <option value="00000000-0000-0000-0000-000000000000">
                    Engineering Mistakes (f6bf2fc9)
                  </option>
                  <option value="00000000-0000-0000-0000-000000000000">
                    Accepted Losses (97f8186c)
                  </option>
                  {journals.data.map((j) => (
                    <option key={j.journalId} value={j.journalId}>
                      {j.journalName} ({j.journalId.split("-")[0]})
                    </option>
                  ))}
                </select>
                <label
                  htmlFor="to"
                  className="block text-sm font-medium text-gray-700"
                >
                  To Journal
                </label>
                <select
                  name="to"
                  id="to"
                  className="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
                >
                  <option>Pick one of these as TO</option>
                  {journals.data.map((j) => (
                    <option key={j.journalId} value={j.journalId}>
                      {j.journalName} ({j.journalId.split("-")[0]})
                    </option>
                  ))}
                </select>
                <div className="mt-2">
                  <button
                    disabled={addEntry.isLoading}
                    type="submit"
                    className="inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                  >
                    {addEntry.isLoading ? <Spinner /> : "Change Ledger"}
                  </button>
                </div>
              </div>
            </div>
          </form>
          <RadioGroup value={selected} onChange={setSelected}>
            <div className="mt-4 grid grid-cols-1 gap-y-6 sm:grid-cols-3 sm:gap-x-4">
              {orderBy(
                journals.data,
                ["journalType.tag", "journalCreated"],
                ["asc", "asc"]
              ).map((j) => (
                <RadioGroup.Option
                  key={j.journalId}
                  value={j.journalId}
                  className={({ checked, active }) =>
                    classNames(
                      checked ? "border-transparent" : "border-gray-300",
                      active ? "ring-2 ring-indigo-500" : "",
                      "relative bg-white border rounded-lg shadow-sm p-4 flex cursor-pointer focus:outline-none"
                    )
                  }
                >
                  {({ checked, active }) => (
                    <>
                      <div className="flex-1 flex">
                        <div className="flex flex-col">
                          <RadioGroup.Label
                            as="span"
                            className="block text-sm font-medium text-gray-900"
                          >
                            {j.journalName}
                          </RadioGroup.Label>
                          <RadioGroup.Description
                            as="span"
                            className="mt-1 flex items-center text-sm text-gray-500"
                          >
                            {j.journalType.tag}
                          </RadioGroup.Description>
                          <RadioGroup.Description
                            as="span"
                            className="mt-6 text-sm font-medium text-gray-900"
                          >
                            {(
                              j.journalBalance[1] / j.journalBalance[2]
                            ).toLocaleString("en-US", {
                              currency: "USD",
                              style: "currency",
                            })}
                          </RadioGroup.Description>
                        </div>
                      </div>
                      <CheckCircleIcon
                        className={classNames(
                          !checked ? "invisible" : "",
                          "h-5 w-5 text-indigo-600"
                        )}
                        aria-hidden="true"
                      />
                      <div
                        className={classNames(
                          active ? "border" : "border-2",
                          checked ? "border-indigo-500" : "border-transparent",
                          "absolute -inset-px rounded-lg pointer-events-none"
                        )}
                        aria-hidden="true"
                      />
                    </>
                  )}
                </RadioGroup.Option>
              ))}
            </div>
          </RadioGroup>
        </div>
      </>
    );
  } else {
    contents = (
      <>
        {contents}
        <div>Loading journals...</div>
      </>
    );
  }

  contents = (
    <>
      {contents}
      <h2 className="text-gray-500 text-xs font-medium uppercase tracking-wide mb-4 mt-4">
        Entries
      </h2>
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
        <div>
          <button
            disabled={doLedgerPayment.isLoading}
            onClick={async () => {
              await doLedgerPayment.mutateAsync();
              navigate({ hash: "Payments" });
            }}
            className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700"
          >
            {doLedgerPayment.isLoading ? <Spinner /> : "Do payment from ledger"}
          </button>
        </div>
      </div>
    </>
  );

  if (selected === null && allEntries.status === "success") {
    contents = (
      <>
        {contents}
        <EntriesFeed
          entries={allEntries.data.filter((e) =>
            filterDate(e, "createdat", interval)
          )}
        />
      </>
    );
  }

  if (selected === null && allEntries.status === "loading") {
    contents = (
      <>
        {contents}
        <div>Loading entries...</div>
      </>
    );
  }

  if (selected !== null) {
    contents = (
      <>
        {contents}
        <JournalFeed journal={selected} interval={interval} />
      </>
    );
  }

  return <>{contents}</>;
}
