import { StyleSheet } from "react-native";
import { transparentColor } from "../Styling/Colors";

export const marginSizes = {
  bottom: 20,
  label: 5,
};

export default StyleSheet.create({
  inputText: {
    backgroundColor: transparentColor,
    borderBottomWidth: 1,
    borderWidth: 0,
    fontSize: 20,
    height: 35,
    marginBottom: marginSizes.bottom,
    paddingHorizontal: 10,
    paddingVertical: 0,
    width: "100%",
  },
  label: {
    fontSize: 16,
    lineHeight: 17,
    marginBottom: marginSizes.label,
    marginHorizontal: 10,
    marginTop: 0,
  },
});
