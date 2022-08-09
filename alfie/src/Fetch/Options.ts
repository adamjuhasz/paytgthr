import { Platform } from "react-native";
import { nativeAppVersion } from "../PlatformSpecific/AppInfo";

export const fetchUA = `Alfie/${nativeAppVersion || "Unknown"} ${Platform.OS}`;

export const commonPostHeaders: RequestInit["headers"] = {
  Accept: "application/json",
};

export const commonGetHeaders: RequestInit["headers"] = {
  Accept: "application/json",
};

export const commonOptions: RequestInit = {};

if (Platform.OS !== "web") {
  Object.assign(commonPostHeaders, {
    "Content-Type": "multipart/form-data", // not needed for web
    "User-Agent": fetchUA,
  });

  Object.assign(commonGetHeaders, {
    "User-Agent": fetchUA,
  });
}

if (Platform.OS === "web") {
  Object.assign(commonOptions, {
    credentials: "include",
    mode: "cors",
    referrerPolicy: "no-referrer-when-downgrade",
  });
}
