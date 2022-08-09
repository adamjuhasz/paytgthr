import {
  LibraryIcon,
  ShoppingCartIcon,
  StopIcon,
  UserCircleIcon,
} from "@heroicons/react/solid";
import { Link } from "react-router-dom";

import { LedgerEntry } from "./Types";
import JournalSpark from "./JournalSpark";
import ShowPurchase from "../Purchases/ShowPurchase";
import ShowPayment from "../Payments/ShowPayment";

const ShowEntryInfo = ({ entry }: { entry: LedgerEntry }) => {
  return (
    <>
      <div>
        On {new Date(entry.createdat).toLocaleString()} (Revision #
        {entry.revision})
      </div>
      <div>
        Entry Id:{" "}
        <Link
          to={`/ledger/entry/${entry.id}`}
          className="text-indigo-500 underline"
        >
          {entry.id.split("-")[0]}
        </Link>{" "}
        JTrx Id:{" "}
        <Link
          to={`/ledger/transaction/${entry.transaction}`}
          className="text-indigo-500 underline"
        >
          {entry.transaction.split("-")[0]}
        </Link>
      </div>
    </>
  );
};

interface Props {
  entries: LedgerEntry[];
}
export default function LedgerEntriesFeed(props: Props): JSX.Element {
  return (
    <div className="flow-root">
      <ul className="-mb-8">
        {props.entries.map((entry, activityItemIdx) => (
          <li key={entry.id}>
            <div className="relative pb-8">
              {activityItemIdx !== props.entries.length - 1 ? (
                <span
                  className="absolute top-5 left-5 -ml-px h-full w-0.5 bg-gray-200"
                  aria-hidden="true"
                />
              ) : null}
              <div className="relative flex items-start space-x-3">
                {entry.fact.kind === "trxadjustment" ? (
                  <>
                    <div>
                      <div className="relative px-1">
                        <div className="h-8 w-8 bg-green-100 rounded-full ring-8 ring-white flex items-center justify-center">
                          <ShoppingCartIcon
                            className="h-5 w-5 text-green-500"
                            aria-hidden="true"
                          />
                        </div>
                      </div>
                    </div>
                    <div className="min-w-0 flex-1">
                      <div>
                        <div className="text-sm">
                          Transaction Adjustment of{" "}
                          {(
                            entry.fact.body[1][1] / entry.fact.body[1][2]
                          ).toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <JournalSpark journal={entry.journal} />
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <ShowEntryInfo entry={entry} />
                        </div>
                      </div>
                      <div className="mt-2 text-sm text-gray-700">
                        <div>
                          <ShowPurchase purchase={entry.fact.body[0]} />
                        </div>
                        <div>
                          Change of{" "}
                          <b>
                            {(
                              entry.fact.body[1][1] / entry.fact.body[1][2]
                            ).toLocaleString("en-US", {
                              style: "currency",
                              currency: "USD",
                            })}
                          </b>{" "}
                          from{" "}
                          {(
                            entry.balance[1] / entry.balance[2] -
                            entry.fact.body[1][1] / entry.fact.body[1][2]
                          ).toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}{" "}
                          to{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                        <div>
                          Current balance:{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                      </div>
                    </div>
                  </>
                ) : entry.fact.kind === "paymentcleared" ? (
                  <>
                    <div>
                      <div className="relative px-1">
                        <div className="h-8 w-8 bg-blue-100 rounded-full ring-8 ring-white flex items-center justify-center">
                          <LibraryIcon
                            className="h-5 w-5 text-blue-500"
                            aria-hidden="true"
                          />
                        </div>
                      </div>
                    </div>
                    <div className="min-w-0 flex-1">
                      <div>
                        <div className="text-sm">
                          Payment Adjustment of{" "}
                          {(
                            entry.fact.body[1][1] / entry.fact.body[1][2]
                          ).toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <JournalSpark journal={entry.journal} />
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <ShowEntryInfo entry={entry} />
                        </div>
                      </div>
                      <div className="mt-2 text-sm text-gray-700">
                        <div>
                          <ShowPayment payment={entry.fact.body[0]} />
                        </div>
                        <div>
                          Change of{" "}
                          <b>
                            {(
                              entry.fact.body[1][1] / entry.fact.body[1][2]
                            ).toLocaleString("en-US", {
                              style: "currency",
                              currency: "USD",
                            })}
                          </b>{" "}
                          from{" "}
                          {(
                            entry.balance[1] / entry.balance[2] -
                            entry.fact.body[1][1] / entry.fact.body[1][2]
                          ).toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}{" "}
                          to{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                        <div>
                          Current balance:{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                      </div>
                    </div>
                  </>
                ) : entry.fact.kind === "initialbalance" ? (
                  <>
                    <div>
                      <div className="relative px-1">
                        <div className="h-8 w-8 bg-yellow-100 rounded-full ring-8 ring-white flex items-center justify-center">
                          <LibraryIcon
                            className="h-5 w-5 text-yellow-500"
                            aria-hidden="true"
                          />
                        </div>
                      </div>
                    </div>
                    <div className="min-w-0 flex-1">
                      <div>
                        <div className="text-sm">Initial Balance</div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <JournalSpark journal={entry.journal} />
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <ShowEntryInfo entry={entry} />
                        </div>
                      </div>
                      <div className="mt-2 text-sm text-gray-700">
                        <div>
                          Change of{" "}
                          <b>
                            {(
                              entry.fact.body[1] / entry.fact.body[2]
                            ).toLocaleString("en-US", {
                              style: "currency",
                              currency: "USD",
                            })}
                          </b>{" "}
                          from{" "}
                          {(
                            entry.balance[1] / entry.balance[2] -
                            entry.fact.body[1] / entry.fact.body[2]
                          ).toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}{" "}
                          to{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                        <div>
                          Current balance:{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                      </div>
                    </div>
                  </>
                ) : entry.fact.kind === "usertransfer" ? (
                  <>
                    <div>
                      <div className="relative px-1">
                        <div className="h-8 w-8 bg-yellow-100 rounded-full ring-8 ring-white flex items-center justify-center">
                          <LibraryIcon
                            className="h-5 w-5 text-yellow-500"
                            aria-hidden="true"
                          />
                        </div>
                      </div>
                    </div>
                    <div className="min-w-0 flex-1">
                      <div>
                        <div className="text-sm">User Initiated Transfer</div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <JournalSpark journal={entry.journal} />
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <ShowEntryInfo entry={entry} />
                        </div>
                      </div>
                      <div className="mt-2 text-sm text-gray-700">
                        <div>
                          Change of{" "}
                          <b>
                            {(
                              entry.fact.body[1] / entry.fact.body[2]
                            ).toLocaleString("en-US", {
                              style: "currency",
                              currency: "USD",
                            })}
                          </b>{" "}
                          from{" "}
                          {(
                            entry.balance[1] / entry.balance[2] -
                            entry.fact.body[1] / entry.fact.body[2]
                          ).toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}{" "}
                          to{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                        <div>
                          Current balance:{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                      </div>
                    </div>
                  </>
                ) : entry.fact.kind === "manual" ? (
                  <>
                    <div>
                      <div className="relative px-1">
                        <div className="h-8 w-8 bg-purple-100 rounded-full ring-8 ring-white flex items-center justify-center">
                          <UserCircleIcon
                            className="h-5 w-5 text-purple-500"
                            aria-hidden="true"
                          />
                        </div>
                      </div>
                    </div>
                    <div className="min-w-0 flex-1">
                      <div>
                        <div className="text-sm">Manual Chewy Transfer</div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <JournalSpark journal={entry.journal} />
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <ShowEntryInfo entry={entry} />
                        </div>
                      </div>
                      <div className="mt-2 text-sm text-gray-700">
                        <div>
                          Change of{" "}
                          <b>
                            {(
                              entry.fact.body[1] / entry.fact.body[2]
                            ).toLocaleString("en-US", {
                              style: "currency",
                              currency: "USD",
                            })}
                          </b>{" "}
                          from{" "}
                          {(
                            entry.balance[1] / entry.balance[2] -
                            entry.fact.body[1] / entry.fact.body[2]
                          ).toLocaleString("en-US", {
                            style: "currency",
                            currency: "USD",
                          })}{" "}
                          to{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                        <div>
                          Current balance:{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                      </div>
                    </div>
                  </>
                ) : (
                  <>
                    <div>
                      <div className="relative px-1">
                        <div className="h-8 w-8 bg-red-100 rounded-full ring-8 ring-white flex items-center justify-center">
                          <StopIcon
                            className="h-5 w-5 text-red-500"
                            aria-hidden="true"
                          />
                        </div>
                      </div>
                    </div>
                    <div className="min-w-0 flex-1">
                      <div>
                        <div className="text-sm">
                          Unknown Adjustment of{" "}
                          {(entry.fact as Record<string, string>).kind}
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <JournalSpark journal={entry.journal} />
                        </div>
                        <div className="mt-0.5 text-sm text-gray-500">
                          <ShowEntryInfo entry={entry} />
                        </div>
                      </div>
                      <div className="mt-2 text-sm text-gray-700">
                        <div>
                          Current balance:{" "}
                          {(entry.balance[1] / entry.balance[2]).toLocaleString(
                            "en-US",
                            { style: "currency", currency: "USD" }
                          )}
                        </div>
                      </div>
                    </div>
                  </>
                )}
              </div>
            </div>
          </li>
        ))}
      </ul>
    </div>
  );
}
