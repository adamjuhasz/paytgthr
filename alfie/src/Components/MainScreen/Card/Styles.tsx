import { StyleSheet } from "react-native";
import { whiteColor } from "../../Styling/Colors";

export const miniCardHeight = 120;
export const miniCardWidth = 190;

export default StyleSheet.create({
  // eslint-disable-next-line react-native/no-color-literals
  alertBall: {
    backgroundColor: "#FF0000",
    borderRadius: 20,
    height: 20,
    position: "absolute",
    right: -5,
    top: -5,
    width: 20,
  },
  container: {
    borderRadius: 12,
    height: miniCardHeight,
    padding: 10,
    width: miniCardWidth,
  },
  flexContainer: {
    alignItems: "flex-start",
    flexDirection: "column",
    height: "100%",
    justifyContent: "flex-end",
    overflow: "hidden",
    width: "100%",
  },
  mainNumberText: {
    fontSize: 45,
  },
  mainTextFormat: {
    fontSize: 30,
    width: "100%",
  },
  purpleBaseText: {
    color: whiteColor,
  },
  subTextFormat: {
    fontSize: 16,
    width: "100%",
  },
});
