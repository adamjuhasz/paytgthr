/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { useParams } from "react-router-dom";

import useGetProgress from "./UseGetProgress";
import useSetProgress from "./UseSetProgress";
import useUpdateProgress from "./UseUpdateProgress";

import DataTable from "../Shared/DataTable";
import UserLink from "../User/UserLink";
import ShowProgram from "./ShowProgram";
import DescriptionList, { Row } from "../Shared/DescriptionList";

export const path = "/referrals/progress/revisions/:pid";

export default function SpecificProgress(_p: unknown): JSX.Element {
  const { pid } = useParams();
  const revisions = useGetProgress(pid || "", pid !== undefined);
  const setProg = useSetProgress(pid || "");
  const updateProg = useUpdateProgress(pid || "");

  if (pid === undefined) {
    return <div>Progress ID missing</div>;
  }

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
        return <span>{val.split("-")[0]}</span>;
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

  const mostRecent = revisions.data[0];

  return (
    <div className="m-2">
      <form
        onSubmit={async (event) => {
          event.preventDefault();
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const elements: Record<string, HTMLInputElement> = event.currentTarget
            .elements as any;
          const referalProgram = elements.referalProgram.value;
          const progressDisplay = elements.progressDisplay.value;
          const progressTag = elements.progress.value;
          const refereeMade = elements.refereeMade.value;
          const programRequires = elements.programRequires.value;
          const referrer = elements.referrer.value;
          const programExpiration = elements.programExpiration.value;
          const progressUpdatedAt = elements.progressUpdatedAt.value;

          let progress = mostRecent.progress;
          switch (progressTag) {
            case "PurchaseCountProgress":
              progress = {
                tag: progressTag,
                refereeMade: parseInt(refereeMade, 10),
                programRequires: parseInt(programRequires, 10),
              };
              break;

            case "ProgramCompleted":
            case "ProgramExpired":
              progress = { tag: progressTag };
              break;
          }

          await setProg.mutateAsync({
            ...mostRecent,
            referalProgram: referalProgram,
            progressDisplay: parseInt(progressDisplay, 10),
            progress: progress,
            referrer: referrer,
            programExpiration: programExpiration,
            progressUpdatedAt: progressUpdatedAt,
            progressRevision: mostRecent.progressRevision + 1,
          });
        }}
      >
        <DescriptionList
          header={mostRecent.progressId}
          subHeader={<UserLink userid={mostRecent.referee} />}
        >
          <Row
            left="ID"
            right={mostRecent.progressId}
            clipboard={mostRecent.progressId}
          />
          <Row
            left="Program"
            right={
              <InputText
                name="referalProgram"
                defaultValue={mostRecent.referalProgram}
              />
            }
          />
          <Row
            left="Progress"
            right={
              <InputText
                name="progressDisplay"
                defaultValue={mostRecent.progressDisplay.toFixed(0)}
              />
            }
          />
          <Row
            left="Status"
            right={
              <>
                <InputText
                  name="progress"
                  defaultValue={mostRecent.progress.tag}
                />
                <div className="mt-2 text-gray-600text-md ">
                  PurchaseCountProgress, ProgramCompleted, or ProgramExpired
                </div>
              </>
            }
          />
          <Row
            left="Purcahses made"
            right={
              <InputText
                name="refereeMade"
                defaultValue={
                  mostRecent.progress.tag === "PurchaseCountProgress"
                    ? mostRecent.progress.refereeMade.toFixed(0)
                    : ""
                }
                placeholder="Required for PurchaseCountProgress, otherwise empty"
              />
            }
          />
          <Row
            left="Purcahses needed"
            right={
              <InputText
                name="programRequires"
                defaultValue={
                  mostRecent.progress.tag === "PurchaseCountProgress"
                    ? mostRecent.progress.programRequires.toFixed(0)
                    : ""
                }
                placeholder="Required for PurchaseCountProgress, otherwise empty"
              />
            }
          />
          <Row
            left="Referrer"
            right={
              <InputText
                name="referrer"
                defaultValue={
                  mostRecent.referrer === null ? "null" : mostRecent.referrer
                }
              />
            }
          />
          <Row
            left="Referree"
            right={<UserLink userid={mostRecent.referee} />}
          />
          <Row
            left="Expires At"
            right={
              <InputText
                name="programExpiration"
                defaultValue={
                  mostRecent.programExpiration === null
                    ? "null"
                    : mostRecent.programExpiration
                }
              />
            }
          />
          <Row left="Created At" right={mostRecent.progressCreatedAt} />
          <Row
            left="Updated at"
            right={
              <InputText
                name="progressUpdatedAt"
                defaultValue={mostRecent.progressUpdatedAt}
              />
            }
          />
          <Row left="Revision" right={mostRecent.progressRevision} />
          <Row
            left="Overwrite"
            right={
              <button
                type="submit"
                className="inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white focus:outline-none focus:ring-2 focus:ring-offset-2 bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"
              >
                {setProg.isLoading
                  ? "..."
                  : setProg.isError
                  ? "Error!"
                  : "Set, doesn't run workflow"}
              </button>
            }
          />
        </DescriptionList>
      </form>
      <form
        onSubmit={async (event) => {
          event.preventDefault();
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const elements: Record<string, HTMLInputElement> = event.currentTarget
            .elements as any;

          const progressTag = elements.progress.value;
          const refereeMade = elements.refereeMade.value;
          const programRequires = elements.programRequires.value;

          let progress = mostRecent.progress;
          switch (progressTag) {
            case "PurchaseCountProgress":
              progress = {
                tag: progressTag,
                refereeMade: parseInt(refereeMade, 10),
                programRequires: parseInt(programRequires, 10),
              };
              break;

            case "ProgramCompleted":
            case "ProgramExpired":
              progress = { tag: progressTag };
              break;
          }

          await updateProg.mutateAsync({
            userid: mostRecent.referee,
            progress: progress,
          });
        }}
      >
        <DescriptionList header="Update progress" subHeader="">
          <Row
            left="Status"
            right={
              <>
                <InputText
                  name="progress"
                  defaultValue={mostRecent.progress.tag}
                />
                <div className="mt-2 text-gray-600text-md ">
                  PurchaseCountProgress, ProgramCompleted, or ProgramExpired
                </div>
              </>
            }
          />
          <Row
            left="Purcahses made"
            right={
              <InputText
                name="refereeMade"
                defaultValue={
                  mostRecent.progress.tag === "PurchaseCountProgress"
                    ? mostRecent.progress.refereeMade.toFixed(0)
                    : ""
                }
                placeholder="Required for PurchaseCountProgress, otherwise empty"
              />
            }
          />
          <Row
            left="Purcahses needed"
            right={
              <InputText
                name="programRequires"
                defaultValue={
                  mostRecent.progress.tag === "PurchaseCountProgress"
                    ? mostRecent.progress.programRequires.toFixed(0)
                    : ""
                }
                placeholder="Required for PurchaseCountProgress, otherwise empty"
              />
            }
          />
          <Row
            left="Update"
            right={
              <button
                type="submit"
                className="inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white focus:outline-none focus:ring-2 focus:ring-offset-2 bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"
              >
                {updateProg.isLoading
                  ? "..."
                  : updateProg.isError
                  ? "Error!"
                  : "Update, will run workflow"}
              </button>
            }
          />
        </DescriptionList>
      </form>
      <DataTable
        columns={columns}
        data={revisions.data}
        filters={[]}
        sortyBy={[]}
      />
    </div>
  );
}

interface InputTextProps {
  name: string;
  defaultValue: string;
  placeholder?: string;
}

const InputText = (props: InputTextProps) => (
  <div className="mt-1 sm:mt-0 sm:col-span-2">
    <div className="max-w-lg flex rounded-md shadow-sm">
      <input
        type="text"
        name={props.name}
        id={props.name}
        autoComplete="off"
        className="flex-1 block w-full min-w-0 rounded-md sm:text-sm placeholder-gray-300 border-gray-300 focus:ring-indigo-500 focus:border-indigo-500"
        placeholder={props.placeholder || props.defaultValue}
        defaultValue={props.defaultValue}
      />
    </div>
  </div>
);
