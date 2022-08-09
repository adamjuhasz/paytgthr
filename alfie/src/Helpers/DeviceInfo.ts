/* eslint-disable @typescript-eslint/no-explicit-any */

import { store } from "../State/Store";

// eslint-disable-next-line @typescript-eslint/require-await
export const generateDeviceInfo = async (): Promise<Record<string, any>> => {
  return {
    device: {
      deviceName: "web",
      brand: "web",
      manufacturer: "web",
      modelName: "web",
      modelId: "web",
      productName: "web",
      osVersion: "web",
      platform: "web",
      platformVersion: "web",
    },
    cellular: {
      carrier: "Unknown",
    },
    permissions: {
      push: false,
      camera: false,
    },
    app: {
      opens: store.getState().appOpens,
      askedForReview: store.getState().askedForReview,
      releaseId: "Unknown",
      appVersion: "Unknown",
      buildVersion: "Unknown",
      pushToken: null,
    },
    version: 1,
  };
};
