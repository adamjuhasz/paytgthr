/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import Survey from "./Survey";

export default {
  title: "Screens/SignUp/Survey",
  component: Survey,
  argTypes: {
    gotoNext: { action: "gotoNext" },
    gotoSurvey: { action: "gotoSurvey" },
  },
} as Meta;

type Args = React.ComponentProps<typeof Survey>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
    >
      <SafeAreaProvider>
        <Survey {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {};
