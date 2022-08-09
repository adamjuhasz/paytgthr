import { FormEvent, PropsWithChildren, ReactNode } from "react";
import { Link } from "react-router-dom";

import useGetReferees from "./UseGetReferees";
import useGetReferralProgress from "./UseGetReferralProgress";
import useGetReferralCode from "./UseGetReferralCode";
import useChangeReferralCode from "./UseChangeReferralCode";
import useChangeLinkReferralCode from "./UseChangeLinkReferralCode";

import Spinner from "../Shared/Spinner";
import UserLink from "./UserLink";
import ShowProgram from "../Referrals/ShowProgram";

interface Props {
  user: string;
}

export default function UserReferral(props: Props): JSX.Element {
  const referees = useGetReferees(props.user);
  const progress = useGetReferralProgress(props.user);
  const code = useGetReferralCode(props.user);

  const changeCode = useChangeReferralCode(props.user);
  const linkCode = useChangeLinkReferralCode(props.user);

  if (referees.status !== "success") {
    return <div>loading referees...</div>;
  }

  if (progress.status !== "success") {
    return <div>loading progress...</div>;
  }

  if (code.status !== "success") {
    return <div>loading code...</div>;
  }

  return (
    <div className="m-2">
      <h3 className="-ml-2 text-lg leading-6 font-medium text-gray-900">
        Code
      </h3>
      {code.data === null ? (
        <div className="bg-white shadow overflow-hidden sm:rounded-md my-4">
          <ul>
            <li>
              <div className="px-4 py-4 sm:px-6">
                <div className="flex items-center justify-between">
                  <div className="ml-1 font-normal text-gray-500">
                    Create a referral code
                  </div>
                  <form
                    className="ml-2 flex-shrink-0 flex"
                    onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                      event.preventDefault();
                      await changeCode.mutateAsync();
                    }}
                  >
                    <button
                      disabled={changeCode.isLoading}
                      type="submit"
                      className="inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 mt-1"
                    >
                      {changeCode.isLoading ? <Spinner /> : "Create code"}
                    </button>
                  </form>
                </div>
              </div>
            </li>
          </ul>
        </div>
      ) : (
        <></>
      )}
      <div className="mt-5 border-t border-b border-gray-200">
        <dl className="divide-y divide-gray-200">
          <Row
            left="Code"
            right={code.data === null ? "<none>" : code.data.referrerCode}
          />
          <Row
            left="Program"
            right={
              code.data === null ? (
                "<none>"
              ) : (
                <ShowProgram program={code.data.codeProgramLinked} />
              )
            }
          />
          <Row
            left="User"
            right={
              code.data === null ? (
                "<none>"
              ) : code.data.codeReferrerId === null ? (
                "Empty"
              ) : (
                <UserLink
                  userid={code.data.codeReferrerId}
                  thisUser={props.user}
                />
              )
            }
          />
          <Row
            left="Created at"
            right={
              code.data === null
                ? "<none>"
                : new Date(code.data.codeCreatedAt).toLocaleString()
            }
          />
        </dl>
      </div>
      <h3 className="mt-4 -ml-2 text-lg leading-6 font-medium text-gray-900">
        Progress
      </h3>
      {progress.data === null ? (
        <div className="bg-white shadow overflow-hidden sm:rounded-md my-4">
          <ul>
            <li>
              <div className="px-4 py-4 sm:px-6">
                <div className="flex items-center justify-between">
                  <div className="ml-1 font-normal text-gray-500">
                    Link to a referral code
                  </div>
                  <div>
                    <form
                      className="ml-2 flex flex-column"
                      onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                        event.preventDefault();
                        // eslint-disable-next-line @typescript-eslint/no-explicit-any
                        const elements = event.currentTarget.elements as any;
                        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
                        const code = (elements.code as HTMLInputElement).value;

                        await linkCode.mutateAsync(code);
                      }}
                    >
                      <input
                        type="text"
                        name="code"
                        id="code"
                        autoComplete="off"
                        className={`mr-2 flex-1 block w-32 min-w-0 rounded-md sm:text-sm placeholder-gray-300 border-gray-300 focus:ring-indigo-500 focus:border-indigo-500`}
                        placeholder="R00AA"
                      />
                      <button
                        type="submit"
                        className="inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 mt-1"
                      >
                        {linkCode.isLoading ? <Spinner /> : "Link"}
                      </button>
                    </form>
                  </div>
                </div>
              </div>
            </li>
          </ul>
        </div>
      ) : (
        <></>
      )}
      {progress.data !== undefined && progress.data !== null ? (
        <div className="mt-5 border-t border-b border-gray-200">
          <dl className="divide-y divide-gray-200">
            <Row
              left="ID"
              right={
                <Link
                  to={`/referrals/progress/revisions/${progress.data.progressId}`}
                  className="text-indigo-500 underline"
                >
                  {progress.data.progressId.split("-")[0]}
                </Link>
              }
            />
            <Row
              left="Program"
              right={<ShowProgram program={progress.data.referalProgram} />}
            />
            <Row
              left="Progress"
              right={
                `${progress.data?.progressDisplay.toFixed(0)}%` || "<None>"
              }
            />
            {progress.data.progress.tag === "PurchaseCountProgress" ? (
              <Row
                left="Purhases left"
                right={`${progress.data.progress.refereeMade} / ${progress.data.progress.programRequires}`}
              />
            ) : progress.data.progress.tag === "ProgramCompleted" ? (
              <Row left="Referral status" right="Referral completed" />
            ) : (
              <Row left="Referral status" right="Referral expired" />
            )}
            <Row
              left="Referrer"
              right={
                progress.data.referrer === null ? (
                  "<None>"
                ) : (
                  <UserLink
                    userid={progress.data.referrer}
                    thisUser={props.user}
                  />
                )
              }
            />
            <Row
              left="Referree"
              right={
                <UserLink
                  userid={progress.data.referee}
                  thisUser={props.user}
                />
              }
            />
            <Row
              left="Expires At"
              right={
                progress.data.programExpiration === null
                  ? "<Never>"
                  : new Date(progress.data.programExpiration).toLocaleString()
              }
            />
            <Row
              left="Created At"
              right={
                progress.data === null
                  ? "<None>"
                  : new Date(progress.data.progressCreatedAt).toLocaleString()
              }
            />
            <Row
              left="Updated at"
              right={
                progress.data === null
                  ? "<None>"
                  : new Date(progress.data.progressUpdatedAt).toLocaleString()
              }
            />
            <Row left="Revision" right={progress.data.progressRevision} />
          </dl>
        </div>
      ) : (
        <></>
      )}
      <h3 className="mt-4 -ml-2 text-lg leading-6 font-medium text-gray-900">
        Referees
      </h3>
      <div className="flex flex-col max-w-full mt-4">
        <div className="-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
          <div className="py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8">
            <div className="shadow overflow-hidden border-b border-gray-200 sm:rounded-lg">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <Th>User</Th>
                    <Th>Program</Th>
                    <Th>Progress</Th>
                    <Th>Created</Th>
                    <Th>Last Update</Th>
                  </tr>
                </thead>
                <tbody>
                  {referees.data.map((referee, entryIdx) => (
                    <tr
                      key={referee.progressId}
                      className={entryIdx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                    >
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                        <UserLink userid={referee.referee} />
                      </td>
                      <Td>
                        <ShowProgram program={referee.referalProgram} />
                      </Td>
                      <Td>{`${referee.progressDisplay}%`}</Td>
                      <Td>
                        {new Date(referee.progressCreatedAt).toLocaleString()}
                      </Td>
                      <Td>
                        {new Date(referee.progressUpdatedAt).toLocaleString()}
                      </Td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

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
