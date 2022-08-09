import React from "react";
import { Image, StyleSheet, Text, View } from "react-native";
import { ThemeProvider } from "../Theming/ThemeContext";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";

import IntroContainer from "./IntroContainer";
import { gradients } from "../Styling/Colors";

const Illus = require("../../../assets/IntroV2/AutoSplit/Artboard.png");

export default {
  title: "Components/IntroContainer",
  component: IntroContainer,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof IntroContainer>;

export const Wiggle: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <IntroContainer {...args}>
        <View style={[StyleSheet.absoluteFill]}>
          <Text>hi!</Text>
        </View>
      </IntroContainer>
    </SafeAreaProvider>
  </ThemeProvider>
);

Wiggle.args = {
  gradient: gradients.sunset,
  pageCount: 5,
  pageIndex: 1,
  background: (
    <Image
      source={Illus}
      resizeMode="repeat"
      style={[StyleSheet.absoluteFill]}
    />
  ),
};

export const Clouds: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <IntroContainer {...args}>
        <View style={[StyleSheet.absoluteFill]}>
          <Text>hi!</Text>
        </View>
      </IntroContainer>
    </SafeAreaProvider>
  </ThemeProvider>
);

Clouds.args = {
  gradient: gradients.hawaii,
  pageCount: 5,
  pageIndex: 1,
  background: (
    <Image
      source={Illus}
      resizeMode="repeat"
      style={[StyleSheet.absoluteFill]}
    />
  ),
};

export const Lines: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <IntroContainer {...args}>
        <View style={[StyleSheet.absoluteFill]}>
          <Text>hi!</Text>
        </View>
      </IntroContainer>
    </SafeAreaProvider>
  </ThemeProvider>
);

Lines.args = {
  gradient: gradients.pinkhaze,
  pageCount: 5,
  pageIndex: 1,
  background: (
    <Image
      source={Illus}
      resizeMode="repeat"
      style={[StyleSheet.absoluteFill]}
    />
  ),
};

export const Glam: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <IntroContainer {...args}>
        <View style={[StyleSheet.absoluteFill]}>
          <Text>hi!</Text>
        </View>
      </IntroContainer>
    </SafeAreaProvider>
  </ThemeProvider>
);

Glam.args = {
  gradient: gradients.deepocean,
  pageCount: 5,
  pageIndex: 1,
  background: (
    <Image
      source={Illus}
      resizeMode="repeat"
      style={[StyleSheet.absoluteFill]}
    />
  ),
};
