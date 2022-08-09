import { useQuery } from "react-query";
import axios from "axios";
import { useParams } from "react-router-dom";
import { uniqBy } from "lodash";
import { useState } from "react";
import { Switch } from "@headlessui/react";
import { ExclamationIcon, XCircleIcon } from "@heroicons/react/solid";
import { uniq } from "lodash";
import { Link } from "react-router-dom";

import { serverlocation } from "../Backend/Server";
import Feed, { AppEvent } from "./AppEventsFeed";
import UserLink from "../User/UserLink";

function classNames(...classes: string[]) {
  return classes.filter(Boolean).join(" ");
}

export const path = "/appevents/user/:uid";

export default function AppEventsForUSer(): JSX.Element {
  const [reverse, setReverse] = useState(false);

  const { uid } = useParams();
  const realPath = path.replace(":uid", uid || "");

  const result = useQuery([path, uid], async (): Promise<AppEvent[]> => {
    const res = await axios.get<AppEvent[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  if (result.status === "success") {
    const usersInvolved = uniq(
      result.data.map((a) => a.evtUser).filter((s) => s !== null)
    );

    const devicesInvolved = uniq(result.data.map((a) => a.evtDevice));

    const unqiue = uniqBy(result.data, "evtCreatedAt");
    return (
      <div className="px-4 pt-8">
        <div>
          <h2 className="text-lg leading-6 font-medium text-gray-900 mb-2">
            Users involved
          </h2>
          {usersInvolved.map((u) => (
            <div key={`${u || ""}`}>
              <UserLink userid={u || ""} />
            </div>
          ))}
        </div>
        <div>
          <h2>Devices involved</h2>
          {devicesInvolved.map((d) => (
            <div key={d}>
              <Link
                to={`/appevents/device/${d}`}
                className="text-indigo-500 underline"
              >
                {d}
              </Link>
            </div>
          ))}
        </div>

        <h3 className="text-lg leading-6 font-medium text-gray-900 mb-2">
          Total events: {result.data.length} Unique Events: {unqiue.length}
        </h3>

        <Switch.Group as="div" className="flex items-center">
          <Switch
            checked={reverse}
            onChange={setReverse}
            className={classNames(
              reverse ? "bg-indigo-600" : "bg-gray-200",
              "relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
            )}
          >
            <span
              aria-hidden="true"
              className={classNames(
                reverse ? "translate-x-5" : "translate-x-0",
                "pointer-events-none inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200"
              )}
            />
          </Switch>
          <Switch.Label as="span" className="ml-3">
            <span className="text-sm font-medium text-gray-900">
              Reverse list
            </span>
          </Switch.Label>
        </Switch.Group>

        <div className="p-4">
          <Feed timeline={reverse ? unqiue.reverse() : unqiue} />
        </div>
      </div>
    );
  }

  if (result.status === "loading") {
    return (
      <div className="rounded-md bg-yellow-50 p-4">
        <div className="flex">
          <div className="flex-shrink-0">
            <ExclamationIcon
              className="h-5 w-5 text-yellow-400"
              aria-hidden="true"
            />
          </div>
          <div className="ml-3">
            <h3 className="text-sm font-medium text-yellow-800">Loading</h3>
            <div className="mt-2 text-sm text-yellow-700">
              <p>Please wait</p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="rounded-md bg-red-50 p-4">
      <div className="flex">
        <div className="flex-shrink-0">
          <XCircleIcon className="h-5 w-5 text-red-400" aria-hidden="true" />
        </div>
        <div className="ml-3">
          <h3 className="text-sm font-medium text-red-800">Error</h3>
          <div className="mt-2 text-sm text-red-700">
            <ul className="list-disc pl-5 space-y-1">
              <li>Refresh please</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  );
}
