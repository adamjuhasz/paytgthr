/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta } from "@storybook/react/types-6-0";

import MiniButton from "./MiniButton";

export default {
  title: "Components/Buttons/MiniButton",
  component: MiniButton,
} as Meta;

export const BaseState = ({ ...args }): JSX.Element => (
  <MiniButton text="" onPress={() => true} {...args} />
);

BaseState.args = {
  text: "Special button 😬",
};
