/* eslint-disable @typescript-eslint/no-explicit-any */
/* global alert */

import { Platform } from "react-native";
import * as Notifications from "expo-notifications";
import * as Device from "expo-device";
import console from "../Global/Console";
import Analytics from "./SegmentAnalytcs";

const trackPush = (notification: Notifications.Notification) => {
  const trigger = notification.request
    .trigger as Notifications.PushNotificationTrigger;

  console.log(
    "Push Notification handler trackPush ",
    notification,
    Platform.OS,
    trigger
  );

  if (Platform.OS === "ios") {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    const payload: Record<string, unknown> = (trigger as any).payload;
    const deliveryToken = payload["CIO-Delivery-Token"] as string | undefined;
    const deliveryId = payload["CIO-Delivery-ID"] as string | undefined;

    void Analytics.track("Push Notification Opened", {
      platform: Platform.OS,
      deliveryToken,
      deliveryId,
    });
  } else if (Platform.OS === "android") {
    void Analytics.track("Push Notification Opened", {
      platform: Platform.OS,
    });
  }
};

const registerforPN = (): void => {
  Notifications.addNotificationReceivedListener((notification) => {
    console.log(
      "Push Notification addNotificationReceivedListener",
      notification.request,
      Platform.OS
    );
    trackPush(notification);
  });

  Notifications.addNotificationResponseReceivedListener((notification) => {
    console.log(
      "Push Notification addNotificationReceivedListener",
      notification.actionIdentifier,
      notification.userText,
      notification.notification.request,
      Platform.OS
    );
    trackPush(notification.notification);
  });

  Notifications.setNotificationHandler({
    // eslint-disable-next-line @typescript-eslint/require-await
    handleNotification: async (notification) => {
      console.log(
        "Push Notification handleNotification",
        notification,
        Platform.OS
      );

      return {
        shouldShowAlert: true,
        shouldPlaySound: false,
        shouldSetBadge: false,
      };
    },
  });
};

export const registerForPushNotificationsAsync = async (): Promise<void> => {
  if (Device.isDevice === false) {
    return;
  }

  const { status: existingStatus } = await Notifications.getPermissionsAsync();
  let finalStatus = existingStatus;

  if (existingStatus !== "granted") {
    const { status } = await Notifications.requestPermissionsAsync();
    finalStatus = status;
  }

  if (finalStatus !== "granted") {
    if (__DEV__ && Platform.OS !== "web") {
      alert("Failed to get push token for push notification!");
    }
    return;
  }

  const expoToken = (await Notifications.getExpoPushTokenAsync()).data;
  const deviceToken = await Notifications.getDevicePushTokenAsync();
  console.log(
    "push notification tokens",
    "expo token",
    expoToken,
    "device token",
    deviceToken
  );
  registerforPN();

  if (Platform.OS === "android") {
    void Notifications.setNotificationChannelAsync("default", {
      name: "default",
      importance: Notifications.AndroidImportance.MAX,
      vibrationPattern: [0, 250, 250, 250],
      lightColor: "#FF231F7C",
    });
  }
};
