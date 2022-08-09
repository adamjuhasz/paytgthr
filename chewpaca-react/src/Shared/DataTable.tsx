/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-explicit-any */

import React, { useEffect, useMemo } from "react";
import { ChevronLeftIcon, ChevronRightIcon } from "@heroicons/react/solid";
import { ArrowSmDownIcon, ArrowSmUpIcon } from "@heroicons/react/outline";
import { CSVLink } from "react-csv";

import { useFilters, usePagination, useSortBy, useTable } from "react-table";

// Define a default UI for filtering
function DefaultColumnFilter(): JSX.Element {
  return <></>;
}

// Define a default UI for filtering
export function ExactMatchColumnFilter({
  column: { filterValue, preFilteredRows, setFilter },
}: any): JSX.Element {
  const count: number = preFilteredRows.length;

  return (
    <input
      value={filterValue || ""}
      onChange={(e) => {
        setFilter(e.target.value || undefined); // Set undefined to remove the filter entirely
      }}
      placeholder={`Search ${count} records...`}
      className="px-2 py-2 shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-52 sm:text-sm border-gray-300 rounded-md"
    />
  );
}

// This is a custom filter UI for selecting
// a unique option from a list
export function SelectColumnFilter({
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
    return [...optionSet].sort();
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

function dateBetweenFilterFn(rows: any[], id: any, filterValues: any) {
  const sd = filterValues[0] ? new Date(filterValues[0]) : undefined;
  const ed = filterValues[1] ? new Date(filterValues[1]) : undefined;

  if (ed || sd) {
    return rows.filter((r) => {
      const cellDate = new Date(r.values[id]);

      if (ed && sd) {
        return cellDate >= sd && cellDate <= ed;
      } else if (sd) {
        return cellDate >= sd;
      } else if (ed) {
        return cellDate <= ed;
      }
      return false;
    });
  } else {
    return rows;
  }
}

export function DateRangeColumnFilter({
  column: { filterValue = [], preFilteredRows, setFilter, id },
}: any): JSX.Element {
  const [min, max] = useMemo(() => {
    let min = preFilteredRows.length
      ? new Date(preFilteredRows[0].values[id])
      : new Date(0);
    let max = preFilteredRows.length
      ? new Date(preFilteredRows[0].values[id])
      : new Date(0);

    preFilteredRows.forEach((row: any) => {
      const rowDate = new Date(row.values[id]);

      min = rowDate <= min ? rowDate : min;
      max = rowDate >= max ? rowDate : max;
    });

    return [min, max];
  }, [id, preFilteredRows]);

  return (
    <div>
      <input
        min={min.toISOString().slice(0, 10)}
        onChange={(e) => {
          const val = e.target.value;
          setFilter((old = []) => [val ? val : undefined, old[1]]);
        }}
        type="date"
        value={filterValue[0] || ""}
      />
      {" to "}
      <input
        max={max.toISOString().slice(0, 10)}
        onChange={(e) => {
          const val = e.target.value;
          setFilter((old = []) => [
            old[0],
            val ? val.concat("T23:59:59.999Z") : undefined,
          ]);
        }}
        type="date"
        value={filterValue[1]?.slice(0, 10) || ""}
      />
    </div>
  );
}

// This is a custom UI for our 'between' or number range
// filter. It uses two number boxes and filters rows to
// ones that have values between the two
export function NumberRangeColumnFilter({
  column: { filterValue = [], preFilteredRows, setFilter, id },
}: any): JSX.Element {
  const [min, max] = useMemo(() => {
    let min = preFilteredRows.length ? preFilteredRows[0].values[id] : 0;
    let max = preFilteredRows.length ? preFilteredRows[0].values[id] : 0;
    preFilteredRows.forEach((row: any) => {
      min = Math.min(row.values[id], min);
      max = Math.max(row.values[id], max);
    });
    return [min, max] as number[];
  }, [id, preFilteredRows]);

  return (
    <div
      style={{
        display: "flex",
      }}
    >
      <input
        value={filterValue[0] || ""}
        type="number"
        onChange={(e) => {
          const val = e.target.value;
          setFilter((old = []) => [
            val ? parseInt(val, 10) : undefined,
            old[1],
          ]);
        }}
        placeholder={`Min (${min})`}
        style={{
          width: "70px",
          marginRight: "0.5rem",
        }}
      />
      to
      <input
        value={filterValue[1] || ""}
        type="number"
        onChange={(e) => {
          const val = e.target.value;
          setFilter((old = []) => [
            old[0],
            val ? parseInt(val, 10) : undefined,
          ]);
        }}
        placeholder={`Max (${max})`}
        style={{
          width: "70px",
          marginLeft: "0.5rem",
        }}
      />
    </div>
  );
}

// This is a custom filter UI that uses a
// slider to set the filter value between a column's
// min and max values
export function SliderColumnFilter({
  column: { filterValue, setFilter, preFilteredRows, id },
}: any): JSX.Element {
  // Calculate the min and max
  // using the preFilteredRows

  const [min, max] = useMemo(() => {
    let min = preFilteredRows.length ? preFilteredRows[0].values[id] : 0;
    let max = preFilteredRows.length ? preFilteredRows[0].values[id] : 0;
    preFilteredRows.forEach((row: any) => {
      min = Math.min(row.values[id], min);
      max = Math.max(row.values[id], max);
    });
    return [min, max];
  }, [id, preFilteredRows]);

  return (
    <>
      <input
        type="range"
        min={min}
        max={max}
        value={filterValue || min}
        onChange={(e) => {
          setFilter(parseInt(e.target.value, 10));
        }}
      />
      <button onClick={() => setFilter(undefined)}>Off</button>
    </>
  );
}

// Define a custom filter filter function!
function filterGreaterThan(rows: any[], id: any, filterValue: any) {
  return rows.filter((row) => {
    const rowValue = row.values[id];
    return rowValue >= filterValue;
  });
}

interface Props<D = any> {
  columns: any[];
  data: D[];
  filters: any[];
  sortyBy: any[];
  filteredDataRef?: React.MutableRefObject<
    React.Dispatch<React.SetStateAction<D[]>>
  >;
}

export default function DataTable(props: Props): JSX.Element {
  // const cancelPayment = useCancelPayment();

  const filterTypes = useMemo(
    () => ({
      dateBetween: dateBetweenFilterFn,
      greaterThan: filterGreaterThan,
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
      columns: props.columns,
      data: props.data,
      initialState: {
        pageIndex: 0,
        pageSize: 100,
        filters: props.filters,
        sortBy: props.sortyBy,
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

  const values = useMemo(() => {
    return rows.map((r: any) => r.values);
  }, [rows]);

  useEffect(() => {
    if (props.filteredDataRef === undefined) {
      return;
    }

    props.filteredDataRef.current(values.map((u: any) => u.userid));
  }, [props.filteredDataRef, values]);

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
                    results{" "}
                    <CSVLink
                      data={values}
                      className="text-rose-500 underline"
                      filename="chewpaca-data.csv"
                    >
                      Download CSV
                    </CSVLink>
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
