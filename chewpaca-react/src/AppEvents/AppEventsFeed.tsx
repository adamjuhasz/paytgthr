/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* This example requires Tailwind CSS v2.0+ */
import { ComponentProps } from "react";
import {
  CheckCircleIcon,
  CurrencyDollarIcon,
  DeviceMobileIcon,
  HeartIcon,
  MapIcon,
  UserIcon,
} from "@heroicons/react/solid";
import { Link } from "react-router-dom";

import UserLink from "../User/UserLink";

export interface AppEvent {
  evtCreatedAt: string;
  evtAppInfo: {
    appBuildVersion: string;
    appHasAskedForReview: null | boolean;
    appOpens: number;
    appPushToken: string;
    appReleaseId: string;
    appVersion: string;
  };
  evtCellularInfo: {
    calCarrier: string;
  };
  evtDevice: string;
  evtDeviceInfo: {
    devBrand: string;
    devManufacturer: string;
    devModelId: string;
    devModelName: string;
    devName: string;
    devOsVersion: string;
    devPlatform: string;
    devPlatformVersion: string;
    devProductName: string;
  };
  evtName: string;
  evtUser: string | null;
  evtUserPermissions: {
    perCamera: boolean;
    perPush: string;
  };
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  evtProperties: null | any;
}

interface Props {
  timeline: AppEvent[];
}

const getBackgroundColor = (eventName: string) => {
  if (eventName.startsWith("Application")) {
    return "bg-blue-500";
  }
  if (eventName.startsWith("Screen")) {
    return "bg-purple-500";
  }
  if (eventName.startsWith("QuestScreen")) {
    return "bg-rose-500";
  }
  if (eventName.endsWith("Attempt")) {
    return "bg-yellow-400";
  }
  if (eventName.endsWith("Entered")) {
    return "bg-lime-500";
  }
  if (eventName.endsWith("Agreed")) {
    return "bg-lime-500";
  }
  if (eventName.endsWith("Error")) {
    return "bg-red-300";
  }
  if (eventName.startsWith("PlaidLink")) {
    return "bg-indigo-500";
  }
  if (eventName.endsWith("Tapped")) {
    return "bg-lime-500";
  }
  switch (eventName) {
    case "Signed Up":
      return "bg-rose-500";
    case "User PhoneVerified":
      return "bg-lime-500";
    case "FundingSource Linked":
      return "bg-lime-500";
    case "ManualLink Entry":
      return "bg-lime-500";
    case "User InviteCode Shared":
      return "bg-lime-500";
    case "User InviteCode Copied":
      return "bg-lime-500";
    case "User CodeRequested sms":
      return "bg-lime-500";
  }
  return "bg-gray-400";
};

interface GetIconProps extends ComponentProps<"svg"> {
  eventName: string;
}
const GetIcon = ({ eventName, ...props }: GetIconProps) => {
  if (eventName.startsWith("QuestScreen")) {
    return <HeartIcon {...props} />;
  }

  if (eventName.startsWith("Screen")) {
    return <MapIcon {...props} />;
  }

  if (eventName.endsWith("Entered")) {
    return <CheckCircleIcon {...props} />;
  }

  if (eventName.startsWith("PlaidLink")) {
    return <CurrencyDollarIcon {...props} />;
  }

  if (eventName.startsWith("Application")) {
    return <DeviceMobileIcon {...props} />;
  }

  return <UserIcon {...props} />;
};

function classNames(...classes: string[]) {
  return classes.filter(Boolean).join(" ");
}

export default function Example({ timeline }: Props): JSX.Element {
  return (
    <>
      <div className="flow-root">
        <ul className="-mb-8 max-w-5xl ml-auto mr-auto">
          {timeline.map((event, eventIdx) => (
            <li key={event.evtCreatedAt}>
              <div className="relative pb-8">
                {eventIdx !== timeline.length - 1 ? (
                  <span
                    className="absolute top-4 left-4 -ml-px h-full w-0.5 bg-gray-200"
                    aria-hidden="true"
                  />
                ) : null}
                <div className="relative flex space-x-3">
                  <div>
                    <span
                      className={classNames(
                        getBackgroundColor(event.evtName),
                        "h-8 w-8 rounded-full flex items-center justify-center ring-8 ring-white"
                      )}
                    >
                      <GetIcon
                        eventName={event.evtName}
                        className="h-5 w-5 text-white"
                        aria-hidden="true"
                      />
                    </span>
                  </div>
                  <div className="min-w-0 flex-1 pt-1.5 flex justify-between space-x-4">
                    <div>
                      <p className="text-sm text-gray-500">{event.evtName}</p>
                      <p className="text-sm text-gray-500">
                        {event.evtDeviceInfo.devPlatform}{" "}
                        {event.evtAppInfo.appVersion} (Release{" "}
                        {event.evtAppInfo.appReleaseId.split("-")[0]})
                      </p>
                      <p className="text-sm text-gray-500">
                        Device:{" "}
                        <Link
                          to={`/appevents/device/${event.evtDevice}`}
                          className="text-indigo-500 underline"
                        >
                          {event.evtDevice}
                        </Link>
                      </p>
                      {event.evtUser !== null ? (
                        <p className="text-sm text-gray-500">
                          User: <UserLink userid={event.evtUser} />
                        </p>
                      ) : (
                        <></>
                      )}
                      {event.evtProperties === null ? (
                        <></>
                      ) : (
                        <pre className="text-sm text-gray-500">
                          {JSON.stringify(event.evtProperties, null, 2)}
                        </pre>
                      )}
                    </div>
                    <div className="text-right text-sm whitespace-nowrap text-gray-500">
                      <time dateTime={event.evtCreatedAt}>
                        {new Date(event.evtCreatedAt).toLocaleString()}
                      </time>
                    </div>
                  </div>
                </div>
              </div>
            </li>
          ))}
        </ul>
      </div>
    </>
  );
}
