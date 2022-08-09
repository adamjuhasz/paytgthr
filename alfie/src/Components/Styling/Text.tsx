/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* global window */
import { StyleSheet } from "react-native";

const isWebStorybook =
  window !== undefined && (window as any).__ISWEBSTORYBOOK__ !== undefined;

export const TextStyles = StyleSheet.create({
  centered: {
    textAlign: "center",
  },
  fontWeight300: {
    fontFamily: "OpenSans_300Light",
    ...(isWebStorybook ? { fontFamily: "Open Sans", fontWeight: "300" } : {}),
  },
  fontWeight400: {
    fontFamily: "OpenSans_400Regular",
    ...(isWebStorybook ? { fontFamily: "Open Sans", fontWeight: "400" } : {}),
  },
  fontWeight500: {
    fontFamily: "OpenSans_600SemiBold",
    ...(isWebStorybook ? { fontFamily: "Open Sans", fontWeight: "600" } : {}),
  },
  fontWeight600: {
    fontFamily: "OpenSans_700Bold",
    ...(isWebStorybook ? { fontFamily: "Open Sans", fontWeight: "700" } : {}),
  },
  nexaHeavy: {
    fontFamily: "Nexa-Heavy",
  },
  nexaLight: {
    fontFamily: "Nexa-Light",
  },
  normalText: {
    fontSize: 18,
  },
});

export default TextStyles;
