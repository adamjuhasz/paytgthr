/* eslint-disable @typescript-eslint/no-explicit-any */

import { Platform } from "react-native";
import Constants from "expo-constants";
import { defaultTo } from "lodash";
import * as Device from "expo-device";
import * as Cellular from "expo-cellular";
import * as Notifications from "expo-notifications";
import { Camera } from "expo-camera";
import * as Updates from "expo-updates";
import * as Application from "expo-application";
import NetInfo from "@react-native-community/netinfo";

import { store } from "../State/Store";

let singltonDeviceToken: Notifications.DevicePushToken | null = null;

const defUnknown = <T>(val: T | null | undefined) => defaultTo(val, "Unknown");

export const generateDeviceInfo = async (): Promise<Record<string, any>> => {
  const { granted: pushGranted } = await Notifications.getPermissionsAsync();
  const { granted: cameraGranted } = await Camera.getCameraPermissionsAsync();
  if (pushGranted && singltonDeviceToken === null && Platform.OS !== "web") {
    void Notifications.getDevicePushTokenAsync().then((token) => {
      singltonDeviceToken = token;
    });
  }

  const installTime = await Application.getInstallationTimeAsync();
  let installReferrer: null | string = null;
  try {
    installReferrer = await Application.getInstallReferrerAsync();
  } catch (e) {
    // eslint-disable-next-line no-undef, no-console
    console.log("Error: installReferrer", e);
  }

  let isRooted = false;
  try {
    isRooted = await Device.isRootedExperimentalAsync();
  } catch (e) {
    // eslint-disable-next-line no-undef, no-console
    console.log("Error: isRootedExperimentalAsync", e);
  }

  const netInfo = await NetInfo.fetch();

  return {
    device: {
      deviceName: defUnknown(Device.deviceName),
      brand: defUnknown(Device.brand),
      manufacturer: defUnknown(Device.manufacturer),
      modelName: defUnknown(Device.modelName),
      modelId: defUnknown(Device.modelId),
      productName: defUnknown(Device.productName),
      osVersion: defUnknown(Device.osVersion),
      platform: Platform.OS,
      platformVersion: Platform.Version.toString(),
      isDevice: defUnknown(Device.isDevice),
      osBuildFingerprint: defUnknown(Device.osBuildFingerprint),
      totalMemory: defUnknown(Device.totalMemory),
      isRooted: isRooted,
    },
    cellular: {
      carrier: defUnknown(Cellular.carrier),
      isoCountryCode: defUnknown(Cellular.isoCountryCode),
      mobileCountryCode: defUnknown(Cellular.mobileCountryCode),
      mobileNetworkCode: defUnknown(Cellular.mobileNetworkCode),
    },
    network: {
      netState: netInfo.type,
      details: netInfo.details,
    },
    permissions: {
      push: pushGranted,
      camera: cameraGranted,
    },
    app: {
      opens: store.getState().appOpens,
      askedForReview: store.getState().askedForReview,
      releaseId: defUnknown(Updates.updateId),
      appVersion: defUnknown(Constants.nativeAppVersion),
      buildVersion: defUnknown(Constants.nativeBuildVersion).toString(),
      pushToken: singltonDeviceToken === null ? null : singltonDeviceToken.data,
      releaseChannel: defUnknown(Updates.releaseChannel),
      installTime: installTime,
      installReferrer: installReferrer,
    },
    version: 1,
  };
};
