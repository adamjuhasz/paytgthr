/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";
import FullScreenAutoBackground from "../../Theming/AutoBackground";

import PopUp from "./PopUp";

export default {
  title: "Screens/SignUp/PopUp",
  component: PopUp,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof PopUp>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <FullScreenAutoBackground>
      <SafeAreaProvider>
        <PopUp {...args} />
      </SafeAreaProvider>
    </FullScreenAutoBackground>
  </ThemeProvider>
);

BaseState.args = {
  headingText: "Heading",
  bodyText: "Line1\n\nLine2",
  buttonText: "Continue",
};
