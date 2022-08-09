import { defaultTo } from "lodash";

import useGetUserModel from "./UseGetUserModel";
import useGetJournals from "./UseGetJournals";
import useGetJournalEntries from "../Ledger/UseGetJournalEntries";
import useGetGroups from "./UseGetGroups";
import useGetPurchases from "./UseGetPurchases";
import useGetCanSpend from "./UseGetCanSpend";
import { useGetRiskScore } from "./UseGetRiskScores";
import useGetScoreInfo from "../Risk/UseGetScoreInfo";

import Spinner from "../Shared/Spinner";
import PurchaseList from "../Purchases/PurchaseTable";

import { currToStr } from "../Shared/Currency";

interface Props {
  user: string;
}

export default function UserDashboard(props: Props): JSX.Element {
  const userModel = useGetUserModel(props.user);
  const journals = useGetJournals(props.user);
  const specificJID =
    journals.status === "success"
      ? journals.data.filter((j) => j.journalType.tag === "PayTgthr")[0] !==
        undefined
        ? journals.data.filter((j) => j.journalType.tag === "PayTgthr")[0]
            .journalId
        : ""
      : "";
  const journalEntries = useGetJournalEntries(specificJID, specificJID !== "");
  const groups = useGetGroups(props.user);
  const activeGroups = defaultTo(groups.data, []).filter(
    (g) => g.status === "groupactive"
  );
  const partnerUID =
    activeGroups.length === 0
      ? ""
      : activeGroups[0].members.filter((m) => m.user !== props.user)[0].user;
  const partnerModel = useGetUserModel(partnerUID, partnerUID !== "");
  const purchases = useGetPurchases(props.user);

  const riskScore = useGetRiskScore(props.user);
  const scoreInfo = useGetScoreInfo(
    riskScore.data?.trustscore || 0,
    riskScore.isSuccess
  );
  const canSpend = useGetCanSpend(props.user);

  const partnerRiskScore = useGetRiskScore(partnerUID, partnerUID !== "");
  const partnerScoreInfo = useGetScoreInfo(
    partnerRiskScore.data?.trustscore || 0,
    partnerRiskScore.isSuccess
  );
  const partnerCanSpend = useGetCanSpend(partnerUID, partnerUID !== "");

  const canSpendNum =
    defaultTo(canSpend.data?.canSpend[1], 0) /
    defaultTo(canSpend.data?.canSpend[2], 1);
  const partnerCanSpendNum =
    defaultTo(partnerCanSpend.data?.canSpend[1], 0) /
    defaultTo(partnerCanSpend.data?.canSpend[2], 1);

  const limitNum =
    defaultTo(scoreInfo.data?.limit[1], 0) /
    defaultTo(scoreInfo.data?.limit[2], 1);
  const partnerLimitNum =
    defaultTo(partnerScoreInfo.data?.limit[1], 0) /
    defaultTo(partnerScoreInfo.data?.limit[2], 1);

  const groupCanSpend = defaultTo(activeGroups[0]?.split, [])
    .map((s) => {
      const ratio = s.ratio.numerator / s.ratio.denominator / 100;
      const groupPart =
        s.user === props.user
          ? canSpendNum / ratio
          : partnerCanSpendNum / ratio;
      return groupPart;
    })
    .reduce((accum, curr) => Math.min(accum, curr), Infinity);

  const groupMax = defaultTo(activeGroups[0]?.split, [])
    .map((s) => {
      const ratio = s.ratio.numerator / s.ratio.denominator / 100;
      const groupPart =
        s.user === props.user ? limitNum / ratio : partnerLimitNum / ratio;
      return groupPart;
    })
    .reduce((accum, curr) => Math.min(accum, curr), Infinity);

  console.log(activeGroups[0]);

  return (
    <div>
      <dl className="mt-5 grid grid-cols-1 gap-5 lg:grid-cols-3">
        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            User Status
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {userModel.status === "success" ? (
              userModel.data.userstate.tag === "UserActive" ? (
                <div className="text-green-500">
                  {userModel.data.userstate.tag}
                </div>
              ) : userModel.data.userstate.tag === "UserClosed" ? (
                <>
                  <div className="text-red-500">
                    {userModel.data.userstate.tag}
                  </div>
                  <div className="text-xl text-red-500">
                    {userModel.data.userstate.contents}
                  </div>
                </>
              ) : userModel.data.userstate.tag === "UserKYCDelay" ? (
                <>
                  <div className="text-red-500">
                    {userModel.data.userstate.tag}
                  </div>
                </>
              ) : (
                <div className="text-yellow-500">
                  {userModel.data.userstate.tag}
                </div>
              )
            ) : (
              <Spinner />
            )}
          </dd>
        </div>
        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            Ledger Balance
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {journalEntries.status === "success" ? (
              journalEntries.data[0] !== undefined ? (
                currToStr(journalEntries.data[0].balance)
              ) : (
                "$0.00"
              )
            ) : (
              <Spinner />
            )}
          </dd>
        </div>
        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            Bank Account Status
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {userModel.status === "success" ? (
              userModel.data.bankverified === null ? (
                <span className="text-red-500">Not entered</span>
              ) : userModel.data.bankverified === false ? (
                <span className="text-yellow-500">Linked</span>
              ) : (
                <span className="text-green-500">Verified</span>
              )
            ) : (
              <Spinner />
            )}
          </dd>
        </div>
        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            KYC Status
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {userModel.status === "success" ? (
              userModel.data.aptokycstatus === null ? (
                <span className="text-yellow-500">Not run</span>
              ) : userModel.data.aptokycstatus.kind === "Passed" ? (
                <span className="text-green-500">
                  {userModel.data.aptokycstatus.kind}
                </span>
              ) : (
                <>
                  <div className="text-red-500">
                    {userModel.data.aptokycstatus.kind}
                  </div>
                  <div className="text-red-500 text-lg">
                    {userModel.data.aptokycstatus.body.join(", ")}
                  </div>
                </>
              )
            ) : (
              <Spinner />
            )}
          </dd>
        </div>
        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            Group Status
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {groups.status === "success" ? (
              activeGroups.length > 0 ? (
                <span className="text-green-500">
                  Has a group (
                  {activeGroups[0].split
                    .map((s) => s.ratio.numerator / s.ratio.denominator)
                    .join(":")}
                  )
                </span>
              ) : (
                <span className="text-red-500">No Group</span>
              )
            ) : (
              <Spinner />
            )}
          </dd>
        </div>

        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            Partner's Bank Account Status
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {partnerModel.status === "success" ? (
              partnerModel.data.bankverified === null ? (
                <span className="text-red-500">Not entered</span>
              ) : partnerModel.data.bankverified === false ? (
                <span className="text-yellow-500">Linked</span>
              ) : (
                <span className="text-green-500">Verified</span>
              )
            ) : (
              <Spinner />
            )}
          </dd>
        </div>

        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            Group limit
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {groupCanSpend.toLocaleString("en-US", {
              style: "currency",
              currency: "USD",
            })}{" "}
            /{" "}
            {groupMax.toLocaleString("en-US", {
              style: "currency",
              currency: "USD",
            })}
          </dd>
        </div>

        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            User's spending
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {canSpend.status === "success"
              ? currToStr(canSpend.data.canSpend)
              : "◎"}{" "}
            /{" "}
            {scoreInfo.status === "success"
              ? currToStr(scoreInfo.data.limit)
              : "◎"}
          </dd>
        </div>

        <div className="px-4 py-5 bg-white shadow rounded-lg overflow-hidden sm:p-6">
          <dt className="text-sm font-medium text-gray-500 truncate">
            Partner's spending
          </dt>
          <dd className="mt-1 text-3xl font-semibold text-gray-900">
            {partnerCanSpend.status === "success"
              ? currToStr(partnerCanSpend.data.canSpend)
              : "◎"}{" "}
            /{" "}
            {partnerScoreInfo.status === "success"
              ? currToStr(partnerScoreInfo.data.limit)
              : "◎"}
          </dd>
        </div>
      </dl>
      <div className="mt-5">
        {purchases.status === "success" ? (
          <PurchaseList
            purchases={purchases.data.slice(0, 5)}
            thisUser={props.user}
          />
        ) : (
          <Spinner />
        )}
      </div>
    </div>
  );
}
