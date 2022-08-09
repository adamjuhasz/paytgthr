/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";

import DotProgress from "./DotProgress";

export default {
  title: "Components/DotProgress",
  component: DotProgress,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof DotProgress>;

export const Base: Story<Args> = ({ ...args }) => (
  <View>
    <DotProgress {...args} />
    <View style={{ height: 5 }} />
    <DotProgress index={0} count={7} />
    <View style={{ height: 5 }} />
    <DotProgress index={1} count={7} />
    <View style={{ height: 5 }} />
    <DotProgress index={2} count={7} />
    <View style={{ height: 5 }} />
    <DotProgress index={3} count={7} />
    <View style={{ height: 5 }} />
    <DotProgress index={4} count={7} />
    <View style={{ height: 5 }} />
    <DotProgress index={5} count={7} />
    <View style={{ height: 5 }} />
    <DotProgress index={6} count={7} />
    <View style={{ height: 5 }} />
    <DotProgress index={7} count={7} />
  </View>
);

Base.args = {
  index: 4,
  count: 7,
};
