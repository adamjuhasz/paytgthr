/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Link } from "react-router-dom";
import useGetAllProgress from "./UseGetAllProgress";

import DataTable from "../Shared/DataTable";
import UserLink from "../User/UserLink";
import ShowProgram from "./ShowProgram";

export const path = "/referrals/progress";

export default function SpecificProgress(_p: unknown): JSX.Element {
  const revisions = useGetAllProgress();
  if (revisions.status !== "success") {
    return <div>{revisions.status}...</div>;
  }

  const columns = [
    {
      Header: "Id",
      accessor: "progressId",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: string = p.cell.value;
        return (
          <Link
            to={`/referrals/progress/revisions/${val}`}
            className="text-indigo-500 underline"
          >
            {val.split("-")[0]}
          </Link>
        );
      },
    },
    {
      Header: "Program",
      accessor: "referalProgram",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: string = p.cell.value;
        return <ShowProgram program={val} />;
      },
    },
    {
      Header: "referee",
      accessor: "referee",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: string = p.cell.value;
        return <UserLink userid={val} />;
      },
    },
    {
      Header: "referrer",
      accessor: "referrer",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: string = p.cell.value;
        return <UserLink userid={val} />;
      },
    },
    {
      Header: "Expiration",
      accessor: "programExpiration",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: null | string = p.cell.value;
        if (val === null) return <></>;
        return <span>{new Date(val).toLocaleString("en-US")}</span>;
      },
    },
    { Header: "progress", accessor: "progress.tag" },
    { Header: "referee Made", accessor: "progress.refereeMade" },
    { Header: "program Requires", accessor: "progress.programRequires" },
    { Header: "progress Display", accessor: "progressDisplay" },
    {
      Header: "Created At",
      accessor: "progressCreatedAt",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: null | string = p.cell.value;
        if (val === null) return <></>;
        return <span>{new Date(val).toLocaleString("en-US")}</span>;
      },
    },
    {
      Header: "Updated At",
      accessor: "progressUpdatedAt",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: null | string = p.cell.value;
        if (val === null) return <></>;
        return <span>{new Date(val).toLocaleString("en-US")}</span>;
      },
    },
    { Header: "Revision", accessor: "progressRevision" },
  ];

  return (
    <div className="m-2">
      <DataTable
        columns={columns}
        data={revisions.data}
        filters={[]}
        sortyBy={[]}
      />
    </div>
  );
}
