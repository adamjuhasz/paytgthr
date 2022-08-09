import "react-native-get-random-values"; // must be first
import { Platform } from "react-native";
import Constants from "expo-constants";
import * as Application from "expo-application";
import * as SecureStore from "expo-secure-store";
import { nanoid } from "nanoid";

let memoDeviceId: null | string = null;
const deviceIdKey = "device_id";

export const nativeAppVersion = Constants.nativeAppVersion;
export const installationId: () => Promise<string> = getDeviceId;

async function save(key: string, value: string): Promise<string> {
  await SecureStore.setItemAsync(key, value);
  return value;
}

async function getDeviceId(): Promise<string> {
  if (memoDeviceId !== null) {
    return memoDeviceId;
  }

  if (Platform.OS === "android") {
    if (Application.androidId !== null) {
      memoDeviceId = Application.androidId;
      return Application.androidId;
    }
  }

  const result = await SecureStore.getItemAsync(deviceIdKey);
  if (result !== null) {
    return result;
  } else {
    const deviceId = nanoid(21);
    memoDeviceId = deviceId;
    await save(deviceIdKey, deviceId);
    return deviceId;
  }
}

void getDeviceId();
