import { StyleSheet, ViewStyle } from "react-native";
import {
  blackColor,
  lightOpacityWhite,
  transparentColor,
  whiteColor,
} from "../Styling/Colors";
import TextStyles from "../Styling/Text";

const actualButtonHeight = 53;
const buttonBottomMargin = 10;
export const buttonHeight = actualButtonHeight + buttonBottomMargin;

const sharedAccountSelectButton: ViewStyle = {
  width: 164,
  height: 41,
  borderRadius: 12,
  justifyContent: "center",
  alignItems: "center",
  backgroundColor: blackColor,
};

export default StyleSheet.create({
  accountButtonHeading: {
    color: whiteColor,
    fontSize: 20,
    marginBottom: 7,
    ...TextStyles.fontWeight600,
  },
  accountButtonText: {
    color: whiteColor,
    fontSize: 16,
    lineHeight: 20,
    ...TextStyles.fontWeight400,
  },
  accountSelectButton: sharedAccountSelectButton,
  accountSelectButtonDisabled: {
    ...sharedAccountSelectButton,
    backgroundColor: lightOpacityWhite,
  },
  accountSelectButtonText: {
    color: whiteColor,
    fontSize: 17,
    lineHeight: 20,
    ...TextStyles.fontWeight600,
  },
  accountbutton: {
    alignItems: "center",
    backgroundColor: lightOpacityWhite,
    borderRadius: 13,
    justifyContent: "center",
    marginBottom: 15,
    paddingVertical: 30,
    width: "100%",
  },
  generalButton: {
    alignItems: "center",
    borderRadius: 30,
    height: actualButtonHeight,
    justifyContent: "center",
    marginBottom: buttonBottomMargin,
    width: "100%",
  },
  generalText: {
    fontSize: 18,
    lineHeight: 21,
    ...TextStyles.fontWeight500,
  },
  secondaryButton: {
    backgroundColor: transparentColor,
  },
});
