/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
/* eslint-disable @typescript-eslint/require-await */

import * as Segment from "expo-analytics-segment";
import * as FirebaseAnalytics from "expo-firebase-analytics";
import { defaultTo, snakeCase } from "lodash";
import * as Device from "expo-device";
import * as Cellular from "expo-cellular";
import { Platform } from "react-native";
import * as Updates from "expo-updates";
import Constants from "expo-constants";
import console from "../Global/Console";
import * as Facebook from "expo-facebook";

import { sendEvent } from "../Actions/Events/SendEvent";
import { Analytics } from "./SegmentAnalyticsType";
import { maskPII } from "../Helpers/MaskPII";

const defUnknown = <T>(val: T | null | undefined) => defaultTo(val, "Unknown");

const analytics: Analytics = {
  middleware: function () {
    return this;
  },
  track: async function (
    event: string,
    properties?: {
      [key: string]: any;
    }
  ) {
    if (__DEV__) {
      // eslint-disable-next-line no-console, no-undef
      console.log("Track:", event, properties);
      return;
    }
    void sendEvent(event, properties);

    const firebaseProps = {
      device_name: defUnknown(Device.deviceName),
      brand: defUnknown(Device.brand),
      manufacturer: defUnknown(Device.manufacturer),
      model_name: defUnknown(Device.modelName),
      model_id: defUnknown(Device.modelId),
      product_name: defUnknown(Device.productName),
      carrier: defUnknown(Cellular.carrier),
      release_id: defUnknown(Updates.updateId),
      platform: Platform.OS,
      platform_version: Platform.Version.toString(),
      ...maskPII(defaultTo(properties, {})),
    };
    void FirebaseAnalytics.logEvent(snakeCase(event), firebaseProps);

    if (properties === undefined) {
      Segment.track(event);
    } else {
      Segment.trackWithProperties(event, properties);
    }

    try {
      switch (event) {
        case "Signed Up":
          void Facebook.logEventAsync("Signed Up FBDirect");
          break;

        case "SSN Entered":
          void Facebook.logEventAsync("Subscribe", {
            _valueToSum: 1.0,
            fb_currency: "USD",
            fb_order_id: "Monthly",
          });
          void Facebook.logEventAsync("SubmitApplication");
          void Facebook.logEventAsync("SSN Entered FBDirect");
          break;

        case "FundingSource Linked":
          void Facebook.logEventAsync("fb_mobile_level_achieved", {
            fb_level: 5,
          });
          void Facebook.logEventAsync("FundingSource Linked FBDirect");
          break;

        case "FundingSource Verified":
          void Facebook.logEventAsync("fb_mobile_add_payment_info");
          void Facebook.logEventAsync("FundingSource Verified FBDirect");
          void Facebook.logEventAsync("fb_mobile_level_achieved", {
            fb_level: 6,
          });
          break;

        case "User InviteCode Shared":
          void Facebook.logEventAsync("InviteCode Shared FBDirect");
          void Facebook.logEventAsync("fb_mobile_level_achieved", {
            fb_level: 3,
          });
          break;

        case "User InviteCode Copied":
          void Facebook.logEventAsync("InviteCode Copied FBDirect");
          break;

        case "QuestScreen WelcomeGift Tapped":
          void Facebook.logEventAsync("fb_mobile_complete_registration", {
            fb_registration_method: "Tgthr",
          });
          void Facebook.logEventAsync("fb_mobile_level_achieved", {
            fb_level: 1,
          });
          void Facebook.logEventAsync("QuestScreen WelcomeGift FBDirect");
          break;

        case "QuestScreen Acceptinvite Tapped":
          void Facebook.logEventAsync("QuestScreen AcceptInvite FBDirect");
          void Facebook.logEventAsync("fb_mobile_level_achieved", {
            fb_level: 2,
          });
          break;

        case "QuestScreen InvitePartner Tapped":
          void Facebook.logEventAsync("QuestScreen InvitePartner FBDirect");
          void Facebook.logEventAsync("fb_mobile_level_achieved", {
            fb_level: 2,
          });
          break;

        case "QuestScreen LinkFS Tapped":
          void Facebook.logEventAsync("QuestScreen LinkFS FBDirect");
          void Facebook.logEventAsync("fb_mobile_level_achieved", {
            fb_level: 4,
          });
          break;

        case "QuestScreen VerifyFS Tapped":
          void Facebook.logEventAsync("QuestScreen VerifyFS FBDirect");
          break;

        default:
          break;
      }
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
      console.error("Facebook logEventAsync error", e.toString());
    }

    try {
      if (Constants.appOwnership === "standalone") {
        console.log("loading branch");
        const { BranchEvent } = await import("expo-branch");

        let branchEvent = new BranchEvent(event);
        switch (event) {
          case "SSN Entered":
            branchEvent = new BranchEvent(BranchEvent.CompleteRegistration);
            break;

          default:
            break;
        }
        try {
          void branchEvent.logEvent();
        } catch (e) {
          console.error("Error: Could not log branch event", e);
        }
      }
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
      console.error("expo banch event", e.toString());
    }
  },
  screen: async function (
    name: string,
    properties?: {
      [key: string]: any;
    }
  ) {
    if (__DEV__) {
      // eslint-disable-next-line no-console, no-undef
      console.log("Screen:", name, properties);
      return;
    }

    void sendEvent(`Screen ${name}`, properties);

    if (properties === undefined) {
      Segment.screen(name);
    } else {
      Segment.screenWithProperties(name, properties);
    }

    void FirebaseAnalytics.setCurrentScreen(name);
  },
  identify: async function (
    user: string,
    traits?: {
      [key: string]: any;
    }
  ) {
    if (__DEV__) {
      // eslint-disable-next-line no-console, no-undef
      console.log("Identify:", user, traits);
      return;
    }

    void FirebaseAnalytics.setUserId(user).then(() => {
      if (traits !== undefined) {
        return FirebaseAnalytics.setUserProperties(traits);
      }
    });

    if (traits === undefined) {
      Segment.identify(user);
    } else {
      Segment.identifyWithTraits(user, traits);
    }

    try {
      void Facebook.setUserIDAsync(user);
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
      console.error("Facebook setUserIDAsync error", e.toString());
    }

    try {
      if (Constants.appOwnership === "standalone") {
        console.log("loading branch");
        const Branch = await import("expo-branch");
        Branch.default.setIdentity(user);
      }
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
      console.error("expo-branch setIdentity error", e.toString());
    }
  },
  setup: async function () {
    Segment.initialize({
      androidWriteKey: "HdiVCBVUOrAn2jaDN0EnqeW00KJHifAk",
      iosWriteKey: "ehbaOy86ERBg7mdii4PpFkHUZIMgGw1k",
    });
  },
  reset: async function () {
    Segment.reset();
    void FirebaseAnalytics.resetAnalyticsData();

    try {
      void Facebook.logOutAsync();
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
      console.error("Facebook logOutAsync error", e.toString());
    }

    try {
      if (Constants.appOwnership === "standalone") {
        const Branch = await import("expo-branch");
        Branch.default.logout();
      }
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
      console.error("expo-branch logout error", e.toString());
    }
  },
  disable: function () {
    void Segment.setEnabledAsync(false);
    void FirebaseAnalytics.setAnalyticsCollectionEnabled(false);
  },
  setTrait: async function (trait: string, value: string) {
    switch (trait) {
      case "city":
      case "country":
      case "dateOfBirth":
      case "email":
      case "firstName":
      case "lastName":
      case "phone":
      case "state":
      case "zip": {
        try {
          const userData: Facebook.UserData = {};
          userData[trait] = value;

          void Facebook.setUserDataAsync(userData);
        } catch (e: any) {
          // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
          console.error("Facebook setUserDataAsync error", e.toString());
        }
      }
    }
  },
};
export default analytics;
