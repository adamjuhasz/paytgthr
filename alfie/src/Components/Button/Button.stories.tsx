/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { View } from "react-native";

import Button from "./Button";

export default {
  title: "Components/Buttons/Button",
  component: Button,
  argTypes: {
    onPress: { action: "onPress" },
  },
} as Meta;

export const Stacked: Story<
  React.ComponentProps<typeof Button>
> = (): JSX.Element => (
  <View>
    <Button text="Primary" style="Primary" onPress={() => 1} />
    <Button
      text="Primary disabled"
      style="Primary"
      disabled
      onPress={() => 1}
    />
    <Button
      text="Primary disabled"
      style="Primary"
      inProgress
      onPress={() => 1}
    />
    <Button text="Secondary" style="Secondary" onPress={() => 1} />
    <Button
      text="Secondary disabled"
      style="Secondary"
      disabled
      onPress={() => 1}
    />
    <Button
      text="Secondary disabled"
      style="Secondary"
      inProgress
      onPress={() => 1}
    />
  </View>
);

export const Primary: Story<React.ComponentProps<typeof Button>> = ({
  ...args
}): JSX.Element => <Button {...args} />;

Primary.args = {
  text: "Special button 😬",
  style: "Primary",
  disabled: false,
};

export const PrimaryDisabled: Story<React.ComponentProps<typeof Button>> = ({
  ...args
}): JSX.Element => <Button {...args} />;

PrimaryDisabled.args = {
  text: "Special button 😬",
  style: "Primary",
  disabled: true,
};

export const Secondary: Story<React.ComponentProps<typeof Button>> = ({
  ...args
}): JSX.Element => <Button {...args} />;

Secondary.args = {
  text: "Not special button 😬",
  style: "Secondary",
  disabled: false,
};

export const SecondaryDisabled: Story<React.ComponentProps<typeof Button>> = ({
  ...args
}): JSX.Element => <Button {...args} />;

SecondaryDisabled.args = {
  text: "Not special button 😬",
  style: "Secondary",
  disabled: true,
};
