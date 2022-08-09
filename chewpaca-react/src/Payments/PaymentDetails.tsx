/* eslint-disable @typescript-eslint/no-unsafe-member-access */
import { FormEvent, ReactNode } from "react";
import { useParams } from "react-router";

import useGetPayment from "./UseGetPayment";
import PaymentTable from "./PaymentTable";
import { currToStr } from "../Shared/Currency";
import UserLink from "../User/UserLink";
import useChangePaymentState from "./UseChangeState";
import Spinner from "../Shared/Spinner";
import JournalSpark from "../Ledger/JournalSpark";

export const path = "/payment/:pid";

export default function PaymentDetails(_props: unknown): JSX.Element {
  const { pid } = useParams();
  const paymentQuery = useGetPayment(pid || "", pid !== undefined);
  const changeState = useChangePaymentState(pid || "");

  if (pid === undefined) {
    return <div>Payment Id missing</div>;
  }

  if (paymentQuery.status !== "success") {
    return <div>Loading</div>;
  }

  const thePayment = paymentQuery.data.payment;

  return (
    <div className="m-2">
      <div className="my-4 border-t border-b border-gray-200">
        <dl className="divide-y divide-gray-200">
          <Row left="ID" right={thePayment.id} />
          <Row left="Routing" right={thePayment.achinfo?.[0] || "<None>"} />
          <Row left="Account" right={thePayment.achinfo?.[1] || "<None>"} />
          <Row left="Amount" right={currToStr(thePayment.amount)} />
          <Row
            left="Created"
            right={new Date(thePayment.createdat).toLocaleString()}
          />
          <Row left="Method" right={thePayment.method.tag} />
          <Row left="Method ID" right={thePayment.methodid} />
          <Row
            left="From Journal"
            right={
              thePayment.fromjournal === null ? (
                "<None>"
              ) : (
                <JournalSpark journal={thePayment.fromjournal} />
              )
            }
          />
          <Row
            left="To Journal"
            right={
              thePayment.tojournal === null ? (
                "<None>"
              ) : (
                <JournalSpark journal={thePayment.tojournal} />
              )
            }
          />
          <Row left="Revision" right={thePayment.revision} />
          <Row left="Status" right={thePayment.status.tag} />
          {thePayment.status.tag === "PaymentFailed" ? (
            <Row left="Failed Reason" right={thePayment.status.contents.tag} />
          ) : (
            <></>
          )}
          <Row left="Subtype" right={thePayment.subtype} />
          <Row left="Text" right={thePayment.text} />
          <Row left="Type" right={thePayment.type} />
          <Row left="User" right={<UserLink userid={thePayment.user} />} />
          <Row
            left="Change State"
            right={
              <form
                className="flex flex-row items-center"
                onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                  event.preventDefault();
                  // eslint-disable-next-line @typescript-eslint/no-explicit-any
                  const elements = event.currentTarget.elements as any;
                  const state = (elements.state as HTMLInputElement).value;
                  await changeState.mutateAsync(state);
                }}
              >
                <select
                  name="state"
                  defaultValue="PaymentCompleted"
                  className="block w-52 pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
                >
                  <option value="PaymentCreated">Created</option>
                  <option value="PaymentPending">Pending</option>
                  <option value="PaymentCompleted">Completed</option>
                  <option value="PaymentFailed ACHR01">Failed ACHR01</option>
                  {/* <option value="PaymentCancelled">Cancelled</option> */}
                </select>
                <button
                  type="submit"
                  className="ml-3 inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white focus:outline-none focus:ring-2 focus:ring-offset-2 bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"
                >
                  {changeState.isLoading ? (
                    <Spinner />
                  ) : changeState.isError ? (
                    "Error"
                  ) : (
                    "Change"
                  )}
                </button>
              </form>
            }
          />
        </dl>
      </div>
      <PaymentTable
        payments={paymentQuery.data.revisions}
        query={paymentQuery}
      />
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
