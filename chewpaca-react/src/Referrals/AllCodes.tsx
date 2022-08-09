/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-explicit-any */
import useGetAllReferralCode from "./UseGetAllReferralCode";

import DataTable from "../Shared/DataTable";
import UserLink from "../User/UserLink";
import ShowProgram from "./ShowProgram";

export const path = "/referrals/codes";

export default function SpecificProgress(_p: unknown): JSX.Element {
  const codes = useGetAllReferralCode();
  if (codes.status !== "success") {
    return <div>{codes.status}...</div>;
  }

  const columns = [
    {
      Header: "Code",
      accessor: "referrerCode",
    },
    {
      Header: "Referrer",
      accessor: "codeReferrerId",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: string = p.cell.value;
        if (val === null) {
          return <span>{"<None>"}</span>;
        }
        return <UserLink userid={val} />;
      },
    },
    {
      Header: "Program",
      accessor: "codeProgramLinked",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: string = p.cell.value;
        return <ShowProgram program={val} />;
      },
    },
    {
      Header: "Created At",
      accessor: "codeCreatedAt",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: null | string = p.cell.value;
        if (val === null) return <></>;
        return <span>{new Date(val).toLocaleString("en-US")}</span>;
      },
    },
  ];

  return (
    <div className="m-2">
      <DataTable
        columns={columns}
        data={codes.data}
        filters={[]}
        sortyBy={[]}
      />
    </div>
  );
}
