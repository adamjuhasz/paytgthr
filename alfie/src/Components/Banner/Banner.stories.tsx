/* eslint-disable react/prop-types */
/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { StyleSheet, View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { useLayout } from "@react-native-community/hooks";

import Banner from "./Banner";

export default {
  title: "Components/Banner",
  component: Banner,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof Banner>;

export const Base: Story<Args> = ({ width: _w, ...args }) => {
  const { onLayout, width } = useLayout();
  return (
    <View style={[StyleSheet.absoluteFill]} onLayout={onLayout}>
      <Banner width={width} {...args} />
    </View>
  );
};

Base.args = {
  color: "SocialRed",
  text: "hi",
};
