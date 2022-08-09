/* eslint-disable @typescript-eslint/no-explicit-any */

import useGetAllInvites from "./UseGetAllInvites";

import DataTable, {
  ExactMatchColumnFilter,
  SelectColumnFilter,
} from "../Shared/DataTable";
import UserLink from "../User/UserLink";

export default function ShowAllInvites(): JSX.Element {
  const invites = useGetAllInvites();

  if (invites.status !== "success") {
    return <div>{invites.status}...</div>;
  }

  const columns = [
    {
      Header: "Code",
      accessor: "inviteCode",
      Filter: ExactMatchColumnFilter,
    },
    {
      Header: "Inviter",
      accessor: "inviter",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: string = p.cell.value;
        return <UserLink userid={val} />;
      },
    },
    { Header: "Status", accessor: "inviteStatus", Filter: SelectColumnFilter },
    {
      Header: "Created",
      accessor: "inviteCreated",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: null | string = p.cell.value;
        if (val === null) return <></>;
        return <span>{new Date(val).toLocaleString("en-US")}</span>;
      },
    },
    {
      Header: "Updated",
      accessor: "inviteUpdated",
      Cell: (p: any) => {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const val: null | string = p.cell.value;
        if (val === null) return <></>;
        return <span>{new Date(val).toLocaleString("en-US")}</span>;
      },
    },
    {
      Header: "Revision",
      accessor: "inviteRevision",
      Filter: SelectColumnFilter,
    },
    { Header: "Group", accessor: "groupCreated" },
  ];

  return (
    <div className="m-2">
      <DataTable
        columns={columns}
        data={invites.data}
        filters={[{ id: "Code", value: "" }]}
        sortyBy={[]}
      />
    </div>
  );
}
