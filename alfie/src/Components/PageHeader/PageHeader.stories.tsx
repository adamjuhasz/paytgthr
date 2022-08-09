/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Text } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import AutoBackground from "../Theming/AutoBackground";

import PageHeader, { LeftChevron } from "./PageHeader";

export default {
  title: "Components/PageHeader",
  component: PageHeader,
  argTypes: {
    textColor: {
      name: "Text Color",
      control: {
        type: "color",
      },
    },
    title: {
      name: "Page Title",
      defaultValue: "Page Title",
    },
    leftAction: {
      action: "leftAction",
    },
    rightAction: {
      actions: "rightAction",
    },
    leftIcon: {
      control: {
        type: null,
      },
    },
    rightIcon: {
      control: {
        type: null,
      },
    },
    style: {
      control: {
        type: null,
      },
    },
  },
} as Meta;

type Args = React.ComponentProps<typeof PageHeader>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <AutoBackground>
      <PageHeader {...args} />
    </AutoBackground>
  </ThemeProvider>
);
BaseState.args = {
  title: "Page Title",
};

export const TextColor: Story<Args> = ({ ...args }: Args): JSX.Element => (
  <ThemeProvider>
    <AutoBackground>
      <PageHeader {...args} />
    </AutoBackground>
  </ThemeProvider>
);
TextColor.args = {
  textColor: "#FF0000",
};

export const WithLeft: Story<Args> = ({ ...args }: Args): JSX.Element => (
  <ThemeProvider>
    <AutoBackground>
      <PageHeader
        {...args}
        title="Page Title"
        leftIcon={<LeftChevron color="#FF0000" size={20} />}
      />
    </AutoBackground>
  </ThemeProvider>
);

export const WithRight: Story<Args> = ({ ...args }: Args): JSX.Element => (
  <ThemeProvider>
    <AutoBackground>
      <PageHeader {...args} rightIcon={<Text>🔆</Text>} />
    </AutoBackground>
  </ThemeProvider>
);

export const WithLeftRight: Story<Args> = ({ ...args }: Args): JSX.Element => (
  <ThemeProvider>
    <AutoBackground>
      <PageHeader
        {...args}
        leftIcon={<Text>📣</Text>}
        rightIcon={<Text>🔆</Text>}
      />
    </AutoBackground>
  </ThemeProvider>
);
