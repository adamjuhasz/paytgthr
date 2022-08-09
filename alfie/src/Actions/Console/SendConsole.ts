/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable no-console */
/* global fetch */

import { store } from "../../State/Store";
import { generateDeviceInfo } from "../../Helpers/DeviceInfo";
import { commonOptions, fetchUA } from "../../Fetch/Options";
import { installationId } from "../../PlatformSpecific/AppInfo";

export const remoteConsole = async (
  level: string,
  args: any[]
): Promise<void> => {
  const baseURL = store.getState().baseURL;
  const userId = store.getState().userInfo.userId;

  let bodyText = "";
  try {
    bodyText = JSON.stringify(args);
  } catch (e) {
    bodyText = "Could not stringify";
  }

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
    text: bodyText,
    userId: userId,
    deviceToken: deviceId,
    level,
    ...deviceInfo,
  };
  await fetch(`${baseURL}/record/console`, {
    ...commonOptions,
    method: "POST",
    headers: { "Content-Type": "application/json", "User-Agent": fetchUA },
    body: JSON.stringify(body),
  });
};
