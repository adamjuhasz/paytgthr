import React from "react";
import {
  ColorValue,
  Pressable,
  SectionListData,
  StyleProp,
  StyleSheet,
  Text,
  View,
  ViewStyle,
} from "react-native";

import TextStyles from "../Styling/Text";
import { Data, Section, SectionText } from "./Types";

export const sectionKey = (section: Section): string => {
  switch (section.sectionType) {
    case "text":
      return section.sectionType;
    case "none":
      return section.key;
  }
};

interface TextProps {
  section: SectionListData<Data, SectionText>;
  style?: StyleProp<ViewStyle>;
}

const SectionHeaderText = (props: TextProps): JSX.Element => {
  return (
    <Pressable style={[styles.container, props.style]}>
      <Text style={[TextStyles.nexaHeavy, styles.text]}>
        {props.section.title}
      </Text>
    </Pressable>
  );
};

const SectionHeaderNone = (_props: unknown): JSX.Element => {
  return <View style={[styles.empty]} />;
};

interface Props {
  section: SectionListData<Data, Section>;
  style?: StyleProp<ViewStyle>;
}

const SectionHeader = (props: Props): JSX.Element => {
  switch (props.section.sectionType) {
    case "text":
      return <SectionHeaderText style={props.style} section={props.section} />;

    case "none":
      return <SectionHeaderNone />;
  }
};

export default SectionHeader;

const menuGrey: ColorValue = "#707171";

const styles = StyleSheet.create({
  container: {
    marginBottom: 10,
    marginTop: 50,
    paddingHorizontal: 10,
  },
  empty: {
    height: 0,
  },
  text: {
    color: menuGrey,
    fontSize: 13,
  },
});
