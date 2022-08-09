/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { View } from "react-native";

import ProgressButton from "./ProgressButton";

export default {
  title: "Components/Buttons/ProgressButton",
  component: ProgressButton,
  argTypes: {
    onPress: { action: "onPress" },
  },
} as Meta;

export const Next: Story<React.ComponentProps<typeof ProgressButton>> = ({
  ...args
}): JSX.Element => (
  <View>
    <ProgressButton {...args} />
  </View>
);

Next.args = {
  index: 3,
  count: 7,
  text: "Next",
  style: "Primary",
  disabled: false,
};

export const Continue: Story<React.ComponentProps<typeof ProgressButton>> = ({
  ...args
}): JSX.Element => (
  <View>
    <ProgressButton {...args} />
  </View>
);

Continue.args = {
  index: 3,
  count: 7,
  text: "Continue",
  style: "Primary",
  disabled: false,
};
