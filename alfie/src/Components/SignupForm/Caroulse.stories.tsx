/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { StyleSheet, Text, View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../Theming/ThemeContext";

import Carousel from "./Carousel";

export default {
  title: "Components/SignUpForm/Carousel",
  component: Carousel,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof Carousel>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => {
  return (
    <ThemeProvider>
      <View style={[StyleSheet.absoluteFill]}>
        <SafeAreaProvider>
          <Carousel {...args} />
        </SafeAreaProvider>
      </View>
    </ThemeProvider>
  );
};

BaseState.args = {
  texts: [<Text key="hi">hi</Text>, "bye", "trye", "bill ney", "hyo"],
};
