import { ReactNode } from "react";
import { useParams } from "react-router-dom";

import useGetPurchase from "./UseGetPurchase";
import { currToStr } from "../Shared/Currency";
import UserLink from "../User/UserLink";

export const path = "/transaction/:tid";

export default function SpecificPurchase(_p: unknown): JSX.Element {
  const { tid } = useParams();
  const purchaseQuery = useGetPurchase(tid || "", tid !== undefined);

  if (tid === undefined) {
    return <div>Transaction ID missing</div>;
  }

  if (purchaseQuery.status !== "success") {
    return <div>Loading purchase</div>;
  }

  const purchase = purchaseQuery.data.purchase;

  return (
    <div className="m-2">
      <div>
        <h3 className="text-lg leading-6 font-medium text-gray-900">
          {purchase.description}
        </h3>
        <p className="mt-1 max-w-2xl text-sm text-gray-500">
          {currToStr(purchase.displayamount)}
        </p>
      </div>
      <div className="mt-5 border-t border-gray-200">
        <dl className="divide-y divide-gray-200">
          <Row left="Id" right={purchase.id} clipboard={purchase.id} />
          <Row
            left="Description"
            right={purchase.description}
            clipboard={purchase.description}
          />
          <Row
            left="Amount"
            right={currToStr(purchase.displayamount)}
            clipboard={currToStr(purchase.displayamount)}
          />
          <Row
            left="Status"
            right={
              <>
                <div>{purchase.state.kind}</div>
                {purchase.state.kind === "trxdeclined" ? (
                  <>
                    <div>{purchase.state.body.kind}</div>
                    <div>
                      {purchase.state.body.kind === "paymentunlinked" ? (
                        purchase.state.body.body?.map((b) => (
                          <UserLink key={b} userid={b} />
                        ))
                      ) : (
                        <></>
                      )}
                    </div>
                  </>
                ) : (
                  <></>
                )}
              </>
            }
          />
          <Row
            left="Purchase date"
            right={new Date(purchase.purchasedat).toLocaleString()}
            clipboard={new Date(purchase.purchasedat).toLocaleString()}
          />
          <Row left="Purchaser" right={<UserLink userid={purchase.userid} />} />
          <Row
            left="Billing amounts"
            right={purchase.billingamounts.map((b) => (
              <div key={b[1]}>
                {new Date(b[1]).toLocaleString()} <b>{currToStr(b[0])}</b>
              </div>
            ))}
          />
          <Row
            left="Group ID"
            right={purchase.groupid === null ? "<None>" : purchase.groupid[0]}
          />
          <Row
            left="Group Revision"
            right={purchase.groupid === null ? "<None>" : purchase.groupid[1]}
          />
          <Row left="Source" right={purchase.source} />
          <Row
            left="Source ID"
            right={purchase.sourceid}
            clipboard={purchase.sourceid}
          />
          <Row left="Source Idempotency" right={purchase.sourceidempotency} />
          <Row
            left="Splits"
            right={purchase.splitamounts.map((s) => (
              <div key={s[0]}>
                <UserLink userid={s[0]} thisUser={purchase.userid} />{" "}
                {s[1].numerator / s[1].denominator}%
              </div>
            ))}
          />

          <Row left="Merchant - Country" right={purchase.merchant.country} />
          <Row left="Merchant - Locality" right={purchase.merchant.locality} />
          <Row left="Merchant - MCC" right={purchase.merchant.mcc} />
          <Row left="Merchant - Name" right={purchase.merchant.name} />
          <Row left="Merchant - Region" right={purchase.merchant.region} />

          <Row left="Details - Context" right={purchase.details.context} />
          <Row
            left="Details - Description"
            right={purchase.details.description}
          />
          <Row
            left="Details - Card Present"
            right={purchase.details.iscardpresent ? "True" : "False"}
          />
          <Row
            left="Details - EMV"
            right={purchase.details.isemv ? "True" : "False"}
          />
          <Row
            left="Details - International"
            right={purchase.details.isinternational ? "True" : "False"}
          />
          <Row
            left="Details - Online"
            right={purchase.details.isonline ? "True" : "False"}
          />
          <Row left="Details - Network" right={purchase.details.network} />
          <Row left="Details - Type" right={purchase.details.type} />

          <Row
            left="Adjustments"
            right={
              <pre>
                {purchase.adjustments
                  .map((a) => JSON.stringify(a, null, 2))
                  .join("\n")}
              </pre>
            }
          />
        </dl>
      </div>
    </div>
  );
}

const Row = (props: {
  left: ReactNode;
  right: ReactNode;
  clipboard?: string;
  button?: { text: string; onClick: () => void };
}) => (
  <div className="py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4">
    <dt className="text-sm font-medium text-gray-500">
      <div className="flex flex-row justify-between">
        <div>{props.left}</div>
        <div>
          {props.clipboard === undefined ? (
            <></>
          ) : (
            <button
              onClick={() => {
                void navigator.clipboard.writeText(props.clipboard || "");
              }}
              className="font-normal text-xs border-2 border-gray-400 text-gray-400 rounded-md p-0.5"
            >
              Copy
            </button>
          )}
        </div>
      </div>
    </dt>
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
