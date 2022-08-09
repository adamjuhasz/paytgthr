/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-explicit-any */

import { useMemo } from "react";
import { Link } from "react-router-dom";
// import { sub } from "date-fns";
import { UseQueryResult } from "react-query";
import { ChevronLeftIcon, ChevronRightIcon } from "@heroicons/react/solid";
import { ArrowSmDownIcon, ArrowSmUpIcon } from "@heroicons/react/outline";

// import useCancelPayment from "./UseCancelPayment";
// import Spinner from "../Shared/Spinner";
import JournalSpark from "../Ledger/JournalSpark";
// import { path as journalPath } from "../Ledger/SpecificJournal";
import UserLink from "../User/UserLink";
import { Payment } from "./Types";
import { useFilters, usePagination, useSortBy, useTable } from "react-table";
import { Currency } from "../Shared/Types";
import { currToStr } from "../Shared/Currency";

export const path = "/payment/:pid";

interface Props {
  payments: Payment[];
  query?: UseQueryResult<unknown, unknown>;
}

// Define a default UI for filtering
function DefaultColumnFilter({
  column: { filterValue, preFilteredRows, setFilter },
}: any) {
  const count: number = preFilteredRows.length;

  return <></>;

  // return (
  //   <input
  //     value={filterValue || ""}
  //     onChange={(e) => {
  //       setFilter(e.target.value || undefined); // Set undefined to remove the filter entirely
  //     }}
  //     placeholder={`Search ${count} records...`}
  //     className="px-2 py-2 shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
  //   />
  // );
}

// This is a custom filter UI for selecting
// a unique option from a list
function SelectColumnFilter({
  column: { filterValue, setFilter, preFilteredRows, id },
}: any): JSX.Element {
  console.log("filter");
  // Calculate the options for filtering
  // using the preFilteredRows
  const options = useMemo<any[]>(() => {
    const optionSet = new Set<any[]>();
    (preFilteredRows as any[]).forEach((row) => {
      optionSet.add(row.values[id]);
    });
    return [...optionSet];
  }, [id, preFilteredRows]);

  // Render a multi-select box
  return (
    <select
      value={filterValue}
      onChange={(e) => {
        setFilter(e.target.value || undefined);
      }}
      onClickCapture={(event) => {
        event?.preventDefault();
      }}
      onClick={(event) => {
        event.preventDefault();
      }}
      className="block w-52 pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
    >
      <option value="">All</option>
      {options.map((option, i) => (
        <option key={i} value={option}>
          {option}
        </option>
      ))}
    </select>
  );
}

export default function PaymentTable(props: Props): JSX.Element {
  // const cancelPayment = useCancelPayment();

  const columns = useMemo(
    () => [
      {
        Header: "Id",
        accessor: "id",
        Cell: (p: any) => (
          <Link
            className="text-rose-500 underline"
            to={`/payment/${p.cell.value as string}`}
          >
            <abbr title={p.cell.value}>{p.cell.value.split("-")[0]}</abbr>
          </Link>
        ),
      },
      {
        Header: "Created",
        accessor: "createdat",
        Cell: (p: any) => (
          <span>{new Date(p.cell.value).toLocaleString()}</span>
        ),
      },
      {
        Header: "User",
        accessor: "user",
        Cell: (p: any) => <UserLink userid={p.cell.value} />,
      },
      {
        Header: "Amount",
        id: "amount",
        accessor: (c: Payment) =>
          (c.type === "DebitFromUser" ? 1 : -1) * (c.amount[1] / c.amount[2]),
        Cell: (p: any) => {
          console.log(p);
          return p.cell.value.toLocaleString("en-US", {
            style: "currency",
            currency: "USD",
          });
        },
      },
      {
        Header: "status",
        accessor: (c: Payment) => c.status.tag,
        id: "status",
        filter: "includes",
        Filter: SelectColumnFilter,
      },
      {
        Header: "Type",
        accessor: "type",
        filter: "includes",
        Filter: SelectColumnFilter,
      },
      {
        Header: "Subtype",
        accessor: "subtype",
        filter: "includes",
        Filter: SelectColumnFilter,
      },
      {
        Header: "From",
        accessor: "fromjournal",
        Cell: (p: any) =>
          p.cell.value === null ? (
            "<None>"
          ) : (
            <JournalSpark journal={p.cell.value} />
          ),
      },
      {
        Header: "To",
        accessor: "tojournal",
        Cell: (p: any) =>
          p.cell.value === null ? (
            "<None>"
          ) : (
            <JournalSpark journal={p.cell.value} />
          ),
      },
    ],
    []
  );

  const filterTypes = useMemo(
    () => ({
      // Or,
    }),
    []
  );

  const defaultColumn = useMemo(
    () => ({
      // Let's set up our default Filter UI
      Filter: DefaultColumnFilter,
    }),
    []
  );

  const tableInstance = useTable(
    {
      columns,
      data: props.payments,
      initialState: {
        pageIndex: 0,
        pageSize: 100,
        filters: [
          { id: "status", value: "PaymentPending" },
          { id: "subtype", value: "NormalPayment" },
        ],
        sortBy: [
          { id: "createdat", desc: true },
          { id: "amount", desc: true },
        ],
      },
      defaultColumn,
      filterTypes,
    },
    useFilters,
    useSortBy,
    usePagination
  );

  const {
    getTableProps,
    getTableBodyProps,
    headerGroups,
    prepareRow,
    page,
    canPreviousPage,
    canNextPage,
    pageCount,
    gotoPage,
    nextPage,
    previousPage,
    rows,
    state: { pageIndex, pageSize },
  } = tableInstance;

  return (
    <div className="flex flex-col" id="payment-table">
      <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
        <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
          <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
            <div className="bg-gray-50 px-4 py-3 flex items-center justify-between border-t border-gray-200 sm:px-6">
              <div className="flex-1 flex justify-between sm:hidden">
                <button
                  disabled={!canPreviousPage}
                  onClick={() => previousPage()}
                  className="relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                >
                  Previous
                </button>
                <button
                  disabled={!canNextPage}
                  onClick={() => nextPage()}
                  className="ml-3 relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                >
                  Next
                </button>
              </div>
              <div className="hidden sm:flex-1 sm:flex sm:items-center sm:justify-between">
                <div>
                  <p className="text-sm text-gray-700">
                    Showing{" "}
                    <span className="font-medium">
                      {pageIndex * pageSize + 1}
                    </span>{" "}
                    to{" "}
                    <span className="font-medium">
                      {Math.min(
                        (pageIndex as number) * (pageSize as number) +
                          (pageSize as number),
                        rows.length
                      )}
                    </span>{" "}
                    of <span className="font-medium">{rows.length}</span>{" "}
                    results
                  </p>
                </div>
                <div>
                  <nav
                    className="relative z-0 inline-flex rounded-md shadow-sm -space-x-px"
                    aria-label="Pagination"
                  >
                    <button
                      disabled={!canPreviousPage}
                      onClick={() => {
                        previousPage();
                        window.scrollTo({ top: 0 });
                      }}
                      className="relative inline-flex items-center px-2 py-2 rounded-l-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                    >
                      <span className="sr-only">Previous</span>
                      <ChevronLeftIcon className="h-5 w-5" aria-hidden="true" />
                    </button>

                    <button
                      onClick={() => {
                        nextPage();
                        document
                          .getElementById("route-container")
                          ?.scrollTo({ top: 0, left: 0 });
                      }}
                      className="relative inline-flex items-center px-2 py-2 rounded-r-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                    >
                      <span className="sr-only">Next</span>
                      <ChevronRightIcon
                        className="h-5 w-5"
                        aria-hidden="true"
                      />
                    </button>
                  </nav>
                </div>
              </div>
            </div>
            <table
              {...getTableProps()}
              className="min-w-full divide-y divide-gray-200"
            >
              <thead className="bg-gray-50">
                {
                  // Loop over the header rows
                  headerGroups.map((headerGroup: any) => (
                    <tr {...headerGroup.getHeaderGroupProps()}>
                      {
                        // Loop over the headers in each row
                        headerGroup.headers.map((column: any) => (
                          <th
                            {...column.getHeaderProps()}
                            scope="col"
                            className={`px-6 py-3 text-left text-xs font-medium uppercase tracking-wider`}
                          >
                            <div
                              className={`flex flex-row ${
                                column.isSorted
                                  ? "text-rose-500"
                                  : "text-gray-500"
                              } ${
                                column.sortedIndex === 0
                                  ? "font-bold"
                                  : column.sortedIndex === 1
                                  ? "font-light"
                                  : ""
                              }`}
                              {...column.getSortByToggleProps()}
                            >
                              {column.render("Header")}{" "}
                              {column.isSortedDesc === undefined ? (
                                <></>
                              ) : column.isSortedDesc ? (
                                <ArrowSmDownIcon className="h-4 w-auto" />
                              ) : (
                                <ArrowSmUpIcon className="h-4 w-auto" />
                              )}
                            </div>
                            <div>
                              {column.canFilter
                                ? column.render("Filter")
                                : "no filter"}
                            </div>
                          </th>
                        ))
                      }
                    </tr>
                  ))
                }
              </thead>
              <tbody {...getTableBodyProps()}>
                {
                  // Loop over the table rows
                  page.map((row: any, entryIdx: number) => {
                    prepareRow(row);
                    return (
                      <tr
                        {...row.getRowProps()}
                        className={
                          entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"
                        }
                      >
                        {
                          // Loop over the rows cells
                          row.cells.map((cell: any) => {
                            return (
                              <td
                                {...cell.getCellProps()}
                                className="px-6 py-4 whitespace-nowrap text-sm text-gray-500"
                              >
                                {
                                  // Render the cell contents
                                  cell.render("Cell")
                                }
                              </td>
                            );
                          })
                        }
                      </tr>
                    );
                  })
                }
              </tbody>
            </table>
            <div className="bg-white px-4 py-3 flex items-center justify-between border-t border-gray-200 sm:px-6">
              <div className="flex-1 flex justify-between sm:hidden">
                <button
                  disabled={!canPreviousPage}
                  onClick={() => previousPage()}
                  className="relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                >
                  Previous
                </button>
                <button
                  disabled={!canNextPage}
                  onClick={() => nextPage()}
                  className="ml-3 relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                >
                  Next
                </button>
              </div>
              <div className="hidden sm:flex-1 sm:flex sm:items-center sm:justify-between">
                <div>
                  <p className="text-sm text-gray-700">
                    Showing{" "}
                    <span className="font-medium">
                      {pageIndex * pageSize + 1}
                    </span>{" "}
                    to{" "}
                    <span className="font-medium">
                      {Math.min(
                        (pageIndex as number) * (pageSize as number) +
                          (pageSize as number),
                        rows.length
                      )}
                    </span>{" "}
                    of <span className="font-medium">{rows.length}</span>{" "}
                    results
                  </p>
                </div>
                <div>
                  <nav
                    className="relative z-0 inline-flex rounded-md shadow-sm -space-x-px"
                    aria-label="Pagination"
                  >
                    <button
                      disabled={!canPreviousPage}
                      onClick={() => {
                        previousPage();
                        window.scrollTo({ top: 0 });
                      }}
                      className="relative inline-flex items-center px-2 py-2 rounded-l-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                    >
                      <span className="sr-only">Previous</span>
                      <ChevronLeftIcon className="h-5 w-5" aria-hidden="true" />
                    </button>

                    <button
                      onClick={() => {
                        nextPage();
                        document
                          .getElementById("route-container")
                          ?.scrollTo({ top: 0, left: 0 });
                      }}
                      className="relative inline-flex items-center px-2 py-2 rounded-r-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                    >
                      <span className="sr-only">Next</span>
                      <ChevronRightIcon
                        className="h-5 w-5"
                        aria-hidden="true"
                      />
                    </button>
                  </nav>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
