import { useQuery } from "react-query";
import axios from "axios";
import { ExclamationIcon, XCircleIcon } from "@heroicons/react/solid";

import { serverlocation } from "../Backend/Server";
import DeviceUserTable from "./DeviceUserTable";

export const path = "/appevents/devices";

type Response = { device: string; user: string | null }[];

export default function AppEventDevices(): JSX.Element {
  const result = useQuery(["appevents", "devices"], async () => {
    const res = await axios.get<Response>(`${serverlocation}${path}`, {
      responseType: "json",
    });
    return res.data;
  });

  if (result.status === "success") {
    return (
      <div className="px-4 pt-8">
        <DeviceUserTable entries={result.data} />
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
