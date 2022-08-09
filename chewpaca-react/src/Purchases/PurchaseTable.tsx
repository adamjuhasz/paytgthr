import { PropsWithChildren } from "react";
import { Purchase } from "./Types";
import UserLink from "../User/UserLink";

interface Props {
  purchases: Purchase[];
  thisUser?: string;
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

export default function PurchaseList({
  purchases,
  thisUser,
}: Props): JSX.Element {
  return (
    <div className="flex flex-col max-w-full">
      <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
        <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
          <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <Th>ID</Th>
                  <Th>Status</Th>
                  <Th>Time</Th>
                  <Th>Description</Th>
                  <Th>Amount</Th>
                  <Th>Purchaser</Th>
                  <Th>Splits</Th>
                </tr>
              </thead>
              <tbody>
                {purchases.map((purchase, entryIdx) => (
                  <tr
                    key={purchase.id}
                    className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                  >
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      <a
                        className="text-rose-500 underline"
                        href={`/transaction/${purchase.id}`}
                      >
                        <abbr title={purchase.id}>
                          {purchase.id.split("-")[0]}
                        </abbr>
                      </a>
                    </td>
                    <Td>
                      {purchase.state.kind === "trxcompleted" ? (
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                          Completed
                        </span>
                      ) : purchase.state.kind === "trxdeclined" ? (
                        <>
                          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                            Declined
                          </span>
                          <br />
                          <span className="mt-1 inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                            {purchase.state.body.kind}
                          </span>
                        </>
                      ) : purchase.state.kind === "trxpending" ? (
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
                          Pending
                        </span>
                      ) : (
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
                          {(purchase.state as Record<string, string>).kind}
                        </span>
                      )}
                    </Td>
                    <Td>{new Date(purchase.purchasedat).toLocaleString()}</Td>
                    <Td>{purchase.description}</Td>
                    <Td>
                      {(
                        purchase.displayamount[1] / purchase.displayamount[2]
                      ).toLocaleString("en-US", {
                        style: "currency",
                        currency: purchase.displayamount[0],
                      })}
                    </Td>
                    <Td>
                      <UserLink userid={purchase.userid} thisUser={thisUser} />
                    </Td>
                    <Td>
                      {purchase.splitamounts.map((u) => (
                        <div>
                          {u[1].numerator}%{" "}
                          <UserLink userid={u[0]} thisUser={thisUser} />
                        </div>
                      ))}
                    </Td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  );
}
