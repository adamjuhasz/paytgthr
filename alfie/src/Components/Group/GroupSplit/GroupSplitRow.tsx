/* eslint-disable react-native/no-color-literals */
import React, { useContext } from "react";
import { Platform, StyleSheet, Switch, Text, View } from "react-native";
import Slider from "@react-native-community/slider";
import { clamp } from "lodash";

import { Category } from "../../../Actions/Groups/CategorySplits";
import TextStyles from "../../Styling/Text";
import { ThemeContext } from "../../Theming/ThemeContext";
import { useScreenSize } from "../../Styling/PhoneSizes";
import { whiteColor } from "../../Styling/Colors";

interface CategoryRowProps {
  cat: Category;
  modify: (cat: Category) => void;
  userInitial: string;
  partnerInitial: string;
}

const CategoryRow = ({
  cat,
  modify,
  userInitial,
  partnerInitial,
}: CategoryRowProps): JSX.Element => {
  const theme = useContext(ThemeContext);
  const { narrowScreen } = useScreenSize();

  const setSplit = (split: number) => {
    modify({ ...cat, userSplit: split });
  };

  const enableCat = (enabled: boolean) => {
    modify({ ...cat, enabled: enabled });
  };

  let icon = (
    <View
      style={[
        CategoryRowStyles.catEmojiBubble,
        {
          backgroundColor: `${theme.textColor}${
            theme.scheme === "light" ? "10" : "20"
          }`,
        },
      ]}
    >
      <Text style={CategoryRowStyles.catEmojiText}>{cat.emoji}</Text>
    </View>
  );
  if (narrowScreen) {
    icon = <></>;
  }

  return (
    <>
      <View
        style={[
          CategoryRowStyles.CatBubble,
          { backgroundColor: `${theme.backgroundColor}00` },
        ]}
      >
        {icon}
        <View style={CategoryRowStyles.bubbleMiddle}>
          <Text
            style={[
              theme.textStyle,
              TextStyles.fontWeight600,
              CategoryRowStyles.titleText,
            ]}
          >
            {cat.title}
          </Text>
          {cat.enabled ? (
            <View style={CategoryRowStyles.sliderRow}>
              <View style={CategoryRowStyles.userInitialBubble}>
                <Text
                  style={[
                    TextStyles.fontWeight400,
                    CategoryRowStyles.partnerInitialText,
                  ]}
                >
                  {userInitial}
                </Text>
              </View>
              <Slider
                style={CategoryRowStyles.slider}
                minimumValue={0}
                maximumValue={100}
                step={5}
                value={cat.userSplit}
                onValueChange={(v) => setSplit(clamp(v, 10, 90))}
                minimumTrackTintColor={cat.enabled ? "#EA4352" : "#DAE1E0"}
                maximumTrackTintColor={cat.enabled ? "#DAE1E0" : "#DAE1E0"}
                thumbTintColor={
                  Platform.OS === "android" ? "#EA4352" : undefined
                }
              />
              <View style={CategoryRowStyles.partnerInitialBubble}>
                <Text
                  style={[
                    TextStyles.fontWeight400,
                    CategoryRowStyles.partnerInitialText,
                  ]}
                >
                  {partnerInitial}
                </Text>
              </View>
            </View>
          ) : (
            <Text
              style={[
                theme.textStyle,
                TextStyles.fontWeight400,
                CategoryRowStyles.descriptionText,
              ]}
            >
              Using{" "}
              <Text style={[TextStyles.fontWeight500]}>Primary Split</Text>{" "}
              currently
            </Text>
          )}
        </View>
        <View style={{}}>
          <Switch
            value={cat.enabled}
            onValueChange={enableCat}
            disabled={cat.id === "Category000"}
            style={CategoryRowStyles.switch}
            trackColor={{ false: "#DAE1E0", true: "#EB4459" }}
            thumbColor={whiteColor}
          />
          {cat.enabled ? (
            <View style={CategoryRowStyles.splitFontBubble}>
              <Text
                style={[
                  TextStyles.fontWeight400,
                  CategoryRowStyles.splitFontText,
                ]}
              >{`${cat.userSplit}:${100 - cat.userSplit}`}</Text>
            </View>
          ) : (
            <></>
          )}
        </View>
      </View>
      {cat.id === "Category000" ? (
        <View style={[CategoryRowStyles.customSplitMArgin]}>
          <Text
            style={[
              theme.textStyle,
              TextStyles.fontWeight600,
              CategoryRowStyles.headerText,
            ]}
          >
            Optional split customization
          </Text>
          <Text
            style={[
              theme.textStyle,
              TextStyles.fontWeight400,
              CategoryRowStyles.catEmojiText,
            ]}
          >
            If you split some purchases differently as a couple, enable
            customized splitting below
          </Text>
        </View>
      ) : (
        <></>
      )}
    </>
  );
};

const CategoryRowStyles = StyleSheet.create({
  CatBubble: {
    alignItems: "center",
    borderRadius: 30,
    flexDirection: "row",
    marginVertical: 5,
    overflow: "hidden",
    padding: 10,
  },
  bubbleMiddle: { flexGrow: 2 },
  catEmojiBubble: {
    alignItems: "center",
    borderRadius: 40,
    height: 40,
    justifyContent: "center",
    marginRight: 10,
    padding: 10,
    width: 40,
  },
  catEmojiText: { fontSize: 15 },
  customSplitMArgin: { marginTop: 20 },
  descriptionText: { color: "#7C8696", fontSize: 14 },
  headerText: { fontSize: 20 },
  partnerInitialBubble: {
    alignItems: "center",
    backgroundColor: "#fef9cb",
    borderRadius: 10,
    height: 20,
    justifyContent: "center",
    width: 20,
  },
  partnerInitialText: { color: "#000000", fontSize: 14 },
  slider: {
    flexGrow: 2,
    marginLeft: Platform.OS === "android" ? -5 : 0,
  },
  sliderRow: {
    alignItems: "center",
    flexDirection: "row",
    paddingRight: 5,
  },
  splitFontBubble: {
    alignItems: "center",
    backgroundColor: "#7C8696",
    borderRadius: 30,
    height: 30,
    justifyContent: "center",
    marginTop: 5,
    width: 50,
  },
  splitFontText: { color: "white", fontSize: 14 },
  switch: { width: 50 },
  titleText: { fontSize: 14 },
  userInitialBubble: {
    alignItems: "center",
    backgroundColor: "#fbdee1",
    borderRadius: 10,
    justifyContent: "center",
    minHeight: 20,
    minWidth: 20,
    paddingHorizontal: 5,
    paddingVertical: 0,
  },
});

export default CategoryRow;
