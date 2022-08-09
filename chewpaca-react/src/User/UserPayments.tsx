import { PropsWithChildren, ReactNode, useEffect, useState } from "react";
import { defaultTo } from "lodash";
import { Link } from "react-router-dom";

import { path as journalPath } from "../Ledger/SpecificJournal";
import useGetPayments from "../Payments/UseGetUserPayments";
import { TimeInterval, filterDate } from "../Shared/IntervalFilter";
import JournalSpark from "../Ledger/JournalSpark";
import useCancelPayment from "../Payments/UseCancelPayment";
import Spinner from "../Shared/Spinner";
import useGetJournals from "./UseGetJournals";
import { reducePayments } from "../Payments/Types";

interface Props {
  user: string;
}

export default function UserPayments(props: Props): JSX.Element {
  const payments = useGetPayments(props.user);
  const [interval, setIntervalVal] = useState<TimeInterval>("30 days");
  const filtered = defaultTo(payments.data, []).filter((p) =>
    filterDate(p, "createdat", interval)
  );
  const cancelPayment = useCancelPayment();
  const journals = useGetJournals(props.user);

  useEffect(() => {
    if (payments.status === "success" && filtered.length === 0) {
      setIntervalVal("All");
    }
  }, [payments.status, filtered, setIntervalVal]);

  if (payments.status !== "success") {
    return <div>Loading payments ({payments.status})...</div>;
  }

  if (journals.status !== "success") {
    return <div>Loading journals ({payments.status})...</div>;
  }

  const ptJournals = journals.data.filter(
    (j) => j.journalType.tag === "PayTgthr"
  );
  const totalLedgeBalance = ptJournals.reduce(
    (accum, curr) => accum + curr.journalBalance[1] / curr.journalBalance[2],
    0
  );
  const pendingPaymentsSum = payments.data
    .filter(
      (p) =>
        p.status.tag === "PaymentPending" || p.status.tag === "PaymentCreated"
    )
    .reduce(reducePayments, 0);

  const upcoming = totalLedgeBalance + pendingPaymentsSum;

  return (
    <>
      <div className="my-4 border-t border-b border-gray-200">
        <dl className="divide-y divide-gray-200">
          <Row
            left="Upcoming payment"
            right={
              <span>
                {upcoming.toLocaleString("en-US", {
                  style: "currency",
                  currency: "USD",
                })}{" "}
                {upcoming === 0 ? "" : upcoming > 0 ? "(Credit)" : "(Debit)"}
              </span>
            }
          />
        </dl>
      </div>

      <div className="flex flex-row items-center mt-1 mb-4 justify-between ">
        <select
          onChange={(e) => {
            const val = e.target.value as TimeInterval;
            setIntervalVal(val);
          }}
          value={interval}
          className="block w-52 pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
        >
          <option value="30 days">30 days of history</option>
          <option value="60 days">60 days of history</option>
          <option value="90 days">90 days of history</option>
          <option value="All">All history</option>
        </select>
        <div></div>
      </div>
      <div className="flex flex-col">
        <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
          <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
            <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <Th>Id</Th>
                    <Th>Actions</Th>
                    <Th>Created</Th>
                    <Th>Status</Th>
                    <Th>Direction</Th>
                    <Th>Type</Th>
                    <Th>Amount</Th>
                    <Th>Bank</Th>
                    <Th>From Jrnl</Th>
                    <Th>To Jrnl</Th>
                  </tr>
                </thead>
                <tbody>
                  {filtered.map((payment, entryIdx) => (
                    <tr
                      key={payment.id}
                      className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                    >
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                        <a
                          className="text-rose-500 underline"
                          href={`/payment/${payment.id}`}
                        >
                          <abbr title={payment.id}>
                            {payment.id.split("-")[0]}
                          </abbr>
                        </a>
                      </td>
                      <Td>
                        {payment.status.tag === "PaymentPending" ||
                        payment.status.tag === "PaymentCreated" ? (
                          <div>
                            <input
                              type="hidden"
                              name="payid"
                              value={payment.id}
                            />
                            <button
                              disabled={cancelPayment.isLoading}
                              onClick={async () => {
                                await cancelPayment.mutateAsync(payment.id);
                                await payments.refetch();
                              }}
                              className="inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                            >
                              {cancelPayment.isLoading ? <Spinner /> : "Cancel"}
                            </button>
                          </div>
                        ) : (
                          <></>
                        )}
                      </Td>
                      <Td>{new Date(payment.createdat).toLocaleString()}</Td>
                      <Td>
                        {payment.status.tag === "PaymentFailed" ? (
                          <>
                            <div>{payment.status.tag}</div>
                            <div>{payment.status.contents.tag}</div>
                          </>
                        ) : (
                          payment.status.tag
                        )}
                      </Td>
                      <Td>{payment.type}</Td>
                      <Td>{payment.subtype}</Td>
                      <Td>
                        ${(payment.amount[1] / payment.amount[2]).toFixed(2)}
                      </Td>
                      <Td>
                        Routing:{" "}
                        {payment.achinfo === null ? "" : payment.achinfo[0]}
                        <br />
                        Acct:{" "}
                        {payment.achinfo === null ? "" : payment.achinfo[1]}
                      </Td>
                      <Td>
                        {payment.fromjournal === null ? (
                          ""
                        ) : (
                          <Link
                            className="text-rose-500"
                            to={journalPath.replace(
                              ":jid",
                              payment.fromjournal
                            )}
                          >
                            <JournalSpark journal={payment.fromjournal} />
                          </Link>
                        )}
                      </Td>
                      <Td>
                        {payment.tojournal === null ? (
                          ""
                        ) : (
                          <Link
                            className="text-rose-500"
                            to={journalPath.replace(":jid", payment.tojournal)}
                          >
                            <JournalSpark journal={payment.tojournal} />
                          </Link>
                        )}
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

const Row = (props: {
  left: ReactNode;
  right: ReactNode;
  button?: { text: string; onClick: () => void };
}) => (
  <div className="py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4">
    <dt className="text-sm font-medium text-gray-500">{props.left}</dt>
    <dd className="mt-1 flex text-sm text-gray-900 sm:mt-0 sm:col-span-2 items-center">
      <span className="flex-grow">{props.right}</span>
      {props.button === undefined ? (
        <></>
      ) : (
        <span className="ml-4 flex-shrink-0">
          <button
            onClick={props.button.onClick}
            type="button"
            className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
          >
            {props.button.text}
          </button>
        </span>
      )}
    </dd>
  </div>
);
