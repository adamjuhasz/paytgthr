/* eslint-disable @typescript-eslint/no-explicit-any */
/* global fetch */

import { store } from "../../State/Store";
import { commonOptions, fetchUA } from "../../Fetch/Options";
import { generateDeviceInfo } from "../../Helpers/DeviceInfo";
import { installationId } from "../../PlatformSpecific/AppInfo";

export const sendEvent = async (
  event: string,
  properties?: { [key: string]: any }
): Promise<void> => {
  const baseURL = store.getState().baseURL;
  const userId = store.getState().userInfo.userId;

  let deviceId = "unknown";
  try {
    deviceId = await installationId();
  } catch (e) {
    // eslint-disable-next-line no-undef, no-console
    console.error("@@@ can't make device id", e);
  }

  let deviceInfo: Record<string, any> = { error: "no device info" };
  try {
    deviceInfo = await generateDeviceInfo();
  } catch (e) {
    // eslint-disable-next-line no-undef, no-console
    console.error("@@@ can't make device info", e);
  }

  const body = {
    name: event,
    userId: userId,
    deviceToken: deviceId,
    properties,
    ...deviceInfo,
  };

  await fetch(`${baseURL}/record/event`, {
    ...commonOptions,
    method: "POST",
    headers: { "Content-Type": "application/json", "User-Agent": fetchUA },
    body: JSON.stringify(body),
  });
};
