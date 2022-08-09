import { StyleSheet } from "react-native";

export const purpleColor = "#A069FF";
export const midnightBlue = "#420f8d";
export const darkPurpleColor = "#26273C";
export const blueColor = "#5EC3F0";
export const yellowColor = "#FCE54D";
export const pinkColor = "#EA4352";
export const greenColor = "#65DBA2";
export const blackColor = "#000000";
export const whiteColor = "#FFFFFF";
export const transparentColor = "transparent";
export const lightOpacityWhite = "rgba(255,255,255,0.2)";
export const midOpacityWhite = "rgba(255,255,255,0.5)";
export const darkOpacityWhite = "rgba(255,255,255,0.6)";
export const veryDarkOpacityWhite = "rgba(255,255,255,0.8)";
export const midOpacityBlack = "rgba(0,0,0,0.5)";
export const veryDarkOpacityBlack = "rgba(0,0,0,0.8)";
export const lightGray = "#F0EFEF";
export const darkGray = "#3E3E3E";
export const grayBorder = "#EBEBEB";
export const grayText = "#95959A";

export const gradients: Record<
  "sunset" | "hawaii" | "pinkhaze" | "deepocean",
  [string, string]
> = {
  sunset: ["#FDB534", "#FC594A"],
  hawaii: ["#3AD79E", "#36B2B9"],
  pinkhaze: ["#F65E93", "#FD7776"],
  deepocean: ["#1FD2DB", "#5D79EC"],
};

export default StyleSheet.create({
  blackBackgroud: {
    backgroundColor: blackColor,
  },
  blackText: {
    color: blackColor,
  },
  blue: {
    backgroundColor: blueColor,
  },
  green: {
    backgroundColor: greenColor,
  },
  pink: {
    backgroundColor: pinkColor,
  },
  purple: {
    backgroundColor: purpleColor,
  },
  yellow: {
    backgroundColor: yellowColor,
  },
});
