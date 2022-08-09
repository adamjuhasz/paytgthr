/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable @typescript-eslint/no-explicit-any */

import { PropsWithChildren } from "react";
import { clamp, isEqual } from "lodash";

import useGetRevisions from "./UseGetUserRevisions";

interface Props {
  user: string;
}

export default function UserPayments(props: Props): JSX.Element {
  const revs = useGetRevisions(props.user);

  if (revs.status === "success") {
    return (
      <div className="flex flex-col max-w-full">
        <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
          <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
            <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <Th>Updated At</Th>
                    <Th>Rev #</Th>
                    <Th>State</Th>
                    <Th>KYC State</Th>
                    <Th>Email</Th>
                    <Th>Email Verified</Th>
                    <Th>Phone</Th>
                    <Th>Phone Verified</Th>
                    <Th>Name</Th>
                    <Th>Street</Th>
                    <Th>Street 2</Th>
                    <Th>City</Th>
                    <Th>State</Th>
                    <Th>Zip</Th>
                    <Th>Bank Name</Th>
                    <Th>Account Name</Th>
                    <Th>Routing #</Th>
                    <Th>Acct #</Th>
                    <Th>FS Verified</Th>
                    <Th>FS Ver Amts</Th>
                    <Th>SSN</Th>
                    <Th>Disclosures</Th>
                    <Th>Legal consent</Th>
                    <Th>Privacy Token</Th>
                    <Th>Password</Th>
                  </tr>
                </thead>
                <tbody>
                  {revs.data.map((rev, entryIdx) => {
                    const preValue =
                      revs.data[
                        clamp(entryIdx + 1, 0, revs.data.length - 1)
                      ][0];
                    return (
                      <tr
                        key={rev[0].revision}
                        className={
                          entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"
                        }
                      >
                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                          {new Date(rev[1]).toLocaleString()}
                        </td>
                        <Td>{rev[0].revision}</Td>
                        <Td
                          prevValue={preValue.userstate}
                          currValue={rev[0].userstate}
                        >
                          {rev[0].userstate.tag}
                        </Td>
                        <Td
                          prevValue={preValue.aptokycstatus}
                          currValue={rev[0].aptokycstatus}
                        >
                          {rev[0].aptokycstatus?.kind}
                        </Td>
                        <Td prevValue={preValue.email}>{rev[0].email}</Td>
                        <Td
                          prevValue={preValue.emailverified}
                          currValue={rev[0].emailverified}
                        >
                          {rev[0].emailverified ? "Yes" : "No"}
                        </Td>
                        <Td prevValue={preValue.phone} currValue={rev[0].phone}>
                          {rev[0].phone}
                        </Td>
                        <Td
                          prevValue={preValue.phoneverified}
                          currValue={rev[0].phoneverified}
                        >
                          {rev[0].phoneverified ? "Yes" : "No"}
                        </Td>
                        <Td
                          prevValue={`${preValue.firstname || ""} ${
                            preValue.lastname || ""
                          }`}
                          currValue={`${rev[0].firstname || ""} ${
                            rev[0].lastname || ""
                          }`}
                        >
                          {rev[0].firstname} {rev[0].lastname}
                        </Td>
                        <Td prevValue={preValue.addressstreet}>
                          {rev[0].addressstreet}
                        </Td>
                        <Td prevValue={preValue.addressstreet2}>
                          {rev[0].addressstreet2}
                        </Td>
                        <Td prevValue={preValue.addresscity}>
                          {rev[0].addresscity}
                        </Td>
                        <Td prevValue={preValue.addressstate}>
                          {rev[0].addressstate}
                        </Td>
                        <Td prevValue={preValue.addresszip}>
                          {rev[0].addresszip}
                        </Td>
                        <Td prevValue={preValue.bankname}>{rev[0].bankname}</Td>
                        <Td prevValue={preValue.bankaccountname}>
                          {rev[0].bankaccountname}
                        </Td>
                        <Td prevValue={preValue.bankrouting}>
                          {rev[0].bankrouting}
                        </Td>
                        <Td prevValue={preValue.bankacount}>
                          {rev[0].bankacount}
                        </Td>
                        <Td
                          prevValue={preValue.bankverified}
                          currValue={rev[0].bankverified}
                        >
                          {rev[0].bankverified ? "Yes" : "No"}
                        </Td>
                        <Td
                          prevValue={preValue.bankverifedamounts}
                          currValue={rev[0].bankverifedamounts}
                        >
                          {rev[0].bankverifedamounts === null
                            ? "<None>"
                            : rev[0].bankverifedamounts.join(", ")}
                        </Td>
                        <Td prevValue={preValue.ssn} currValue={rev[0].ssn}>
                          {rev[0].ssn === null ? "" : rev[0].ssn}
                        </Td>
                        <Td
                          prevValue={preValue.constentok}
                          currValue={rev[0].constentok}
                        >
                          {rev[0].constentok}
                        </Td>
                        <Td
                          prevValue={preValue.dislcosureok}
                          currValue={rev[0].dislcosureok}
                        >
                          {rev[0].dislcosureok}
                        </Td>
                        <Td prevValue={preValue.privacyaccttoken}>
                          {rev[0].privacyaccttoken}
                        </Td>
                        <Td
                          prevValue={preValue.password}
                          currValue={rev[0].password}
                        >
                          {rev[0].password === null
                            ? "<None>"
                            : rev[0].password}
                        </Td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return <div>Loading revisions...</div>;
}

interface TableProps {
  className?: string;
  prevValue?: any;
  currValue?: any;
}

const Th = (props: PropsWithChildren<TableProps>) => (
  <th
    scope="col"
    className={`px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider ${
      props.className || ""
    }`}
  >
    {props.children}
  </th>
);

const defaultTo = (a: any, b: any) => {
  if (a === undefined) {
    return b;
  }
  return a;
};

const Td = (props: PropsWithChildren<TableProps>) => (
  <td
    className={`px-6 py-4 whitespace-nowrap text-sm text-gray-500 ${
      isEqual(
        defaultTo(props.prevValue, props.children),
        defaultTo(props.currValue, props.children)
      )
        ? ""
        : "bg-yellow-100"
    } ${props.className || ""}`}
  >
    {props.children}
  </td>
);
