import React from "react";
import {
  LayoutChangeEvent,
  SectionList,
  SectionListData,
  SectionListRenderItemInfo,
  StyleProp,
  TextStyle,
  ViewStyle,
} from "react-native";
import { defaultTo } from "lodash";

import Item from "./TextItem";
import SectionHeader, { sectionKey } from "./SectionHeader";
import { Data, Section, TextData } from "./Types";

interface Props {
  data: SectionListData<Data, Section>[];
  style?: StyleProp<ViewStyle>;
  headerStyle?: StyleProp<ViewStyle>;
  itemStyle?: StyleProp<ViewStyle>;
  sectionStyle?: StyleProp<ViewStyle>;
  textStyle?: StyleProp<TextStyle>;
  helpTextStyle?: StyleProp<TextStyle>;
  onLayout?: (event: LayoutChangeEvent) => void;
  ListFooterComponent?:
    | React.ComponentType<unknown>
    | React.ReactElement
    | null;
}

const SectionedList = (props: Props): JSX.Element => {
  return (
    <SectionList
      onLayout={props.onLayout}
      style={props.style}
      ListFooterComponent={props.ListFooterComponent}
      renderItem={(p) => {
        switch (p.item.type) {
          case "text":
            return (
              <Item
                {...(p as SectionListRenderItemInfo<TextData, Section>)}
                style={props.itemStyle}
                textStyle={props.textStyle}
                helpTextStyle={props.helpTextStyle}
              />
            );
          case "node":
            return p.item.node;
        }
      }}
      keyExtractor={(item) => {
        switch (item.type) {
          case "text":
            return `${item.text}-${defaultTo(item.helpText, "")}-${defaultTo(
              item.key,
              ""
            )}`;
          case "node":
            return item.key;
        }
      }}
      renderSectionHeader={(p) => (
        <SectionHeader
          key={sectionKey(p.section)}
          {...p}
          style={[props.headerStyle, props.sectionStyle]}
        />
      )}
      stickySectionHeadersEnabled={false}
      showsVerticalScrollIndicator={true}
      sections={props.data}
    />
  );
};

export default SectionedList;
