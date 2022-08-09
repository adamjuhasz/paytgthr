import { Link, useLocation, useNavigate, useParams } from "react-router-dom";
import { Helmet } from "react-helmet";
import { defaultTo } from "lodash";

import UserGroups from "./UserGroups";
import UserModelView from "./UserModelView";
import UserBoosts from "./UserBoosts";
import AppEventsForUSer from "../AppEvents/AppEventsForUser";
import UserNotes from "./UserNotes";
import UserKYC from "./UserKYC";
import UserCodes from "./UserCodes";
import UserLedger from "./UserLedger";
import UserRisk from "./UserRisk";
import UserCards from "./UserCards";
import UserPurchases from "./UserPurchases";
import UserPayments from "./UserPayments";
import UserRevisions from "./UserRevisions";
import UserDashboard from "./UserDashboard";
import UserReferral from "./UserReferral";

import useGetUserModel from "./UseGetUserModel";

export const path = "/user/:uid";

export default function UserView(): JSX.Element {
  const tabs = [
    "Dashboard",
    "Info",
    "App Events",
    "KYC",
    "Ledger",
    "Risk Scores",
    "Notes",
    "Cards",
    "Purchases",
    "Payments",
    "Boosts",
    "Referral",
    "Groups",
    "Revisions",
    "Codes",
  ] as const;

  const { uid } = useParams();
  const { hash } = useLocation();
  const navigate = useNavigate();
  const model = useGetUserModel(uid || "", uid !== undefined);

  let normalizedHash = hash
    .replace("#", "")
    .replaceAll("%20", " ") as typeof tabs[number];

  if (uid === undefined) {
    return <div>Errpr: no user param</div>;
  }

  let contents = <></>;
  switch (normalizedHash) {
    case "" as typeof tabs[number]:
    case "Dashboard":
      normalizedHash = "Dashboard";
      contents = <UserDashboard user={uid} />;
      break;

    case "Info":
      contents = <UserModelView user={uid} />;
      break;

    case "Groups":
      contents = <UserGroups user={uid} />;
      break;

    case "Boosts":
      contents = <UserBoosts user={uid} />;
      break;

    case "App Events":
      contents = <AppEventsForUSer />;
      break;

    case "Notes":
      contents = <UserNotes user={uid} />;
      break;

    case "KYC":
      contents = <UserKYC user={uid} />;
      break;

    case "Codes":
      contents = <UserCodes user={uid} />;
      break;

    case "Ledger":
      contents = <UserLedger user={uid} />;
      break;

    case "Risk Scores":
      contents = <UserRisk user={uid} />;
      break;

    case "Cards":
      contents = <UserCards user={uid} />;
      break;

    case "Purchases":
      contents = <UserPurchases user={uid} />;
      break;

    case "Payments":
      contents = <UserPayments user={uid} />;
      break;

    case "Revisions":
      contents = <UserRevisions user={uid} />;
      break;

    case "Referral":
      contents = <UserReferral user={uid} />;
      break;

    default:
      break;
  }

  return (
    <>
      {model.data !== undefined ? (
        <Helmet>
          <title>
            {defaultTo(model.data.firstname, "")}{" "}
            {defaultTo(model.data.lastname, "")} - {normalizedHash}
          </title>
        </Helmet>
      ) : (
        <></>
      )}
      <div className="px-4 pt-8">
        <div className="pb-4">
          <div className="lg:hidden">
            <h3 className="text-lg leading-6 font-medium text-gray-900">
              {model.data?.firstname} {model.data?.lastname}
            </h3>
            <label htmlFor="tabs" className="sr-only">
              Select a tab
            </label>
            {/* Use an "onChange" listener to redirect the user to the selected tab URL. */}
            <select
              id="tabs"
              name="tabs"
              className="block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
              defaultValue={tabs.find((tab) => normalizedHash === tab)}
              onChange={(e) => {
                navigate({ hash: e.target.value });
              }}
            >
              {tabs.map((tab) => (
                <option key={tab}>{tab}</option>
              ))}
            </select>
          </div>
          <div className="hidden lg:block">
            <h3 className="text-lg leading-6 font-medium text-gray-900">
              {model.data?.firstname} {model.data?.lastname}
            </h3>
            <div className="border-b border-gray-200">
              <nav
                className="-mb-px flex space-x-8 overflow-scroll"
                aria-label="Tabs"
              >
                {tabs.map((tab) => (
                  <Link
                    to={{ hash: tab }}
                    key={tab}
                    className={`${
                      normalizedHash === tab
                        ? "border-indigo-500 text-indigo-600"
                        : "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300"
                    }
                  whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm`}
                  >
                    {tab}
                  </Link>
                ))}
              </nav>
            </div>
          </div>
        </div>

        {contents}
      </div>
    </>
  );
}
