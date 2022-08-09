import { useDebugValue } from "react";
import { useDimensions } from "@react-native-community/hooks";

export type DeviceClass = "miniPhone" | "phone" | "tablet" | "desktop";

export const useScreenSize = (): {
  narrowScreen: boolean;
  shortScreen: boolean;
  deviceClass: DeviceClass;
  width: number;
  height: number;
} => {
  const { width, height } = useDimensions().window;

  let isShort = false;
  let isNarrow = false;
  if (height <= 568) {
    //iPhone 4-inch (iPhone 5, iPhone 5S, iPhone 5C, iPhone SE)
    isShort = true;
  }

  if (width <= 320) {
    isNarrow = true;
  }

  let deviceClass: DeviceClass = "phone";
  if (width <= 320) {
    deviceClass = "miniPhone";
  } else if (width <= 480) {
    deviceClass = "phone";
  } else if (width <= 1024) {
    deviceClass = "tablet";
  } else {
    deviceClass = "desktop";
  }

  useDebugValue(
    `Device is a ${deviceClass}${isShort ? " and is short" : ""}${
      isNarrow ? " and is narrow" : ""
    }`
  );

  return {
    narrowScreen: isNarrow,
    shortScreen: isShort,
    deviceClass,
    width,
    height,
  };
};
