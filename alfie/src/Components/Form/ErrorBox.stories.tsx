/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import FullScreenAutoBackground from "../Theming/AutoBackground";
import ErrorBox from "./ErrorBox";

export default {
  title: "Components/ErrorBox",
  component: ErrorBox,
} as Meta;

export const Stacked: Story<
  React.ComponentProps<typeof ErrorBox>
> = (): JSX.Element => {
  return (
    <ThemeProvider>
      <FullScreenAutoBackground style={{ paddingTop: 20 }}>
        <ErrorBox text="Bad thing" icon="Warning" />
        <ErrorBox text="Bad thing" icon="Security" />
      </FullScreenAutoBackground>
    </ThemeProvider>
  );
};

export const Security: Story<React.ComponentProps<typeof ErrorBox>> = ({
  ...args
}) => <ErrorBox {...args} />;
Security.args = {
  text: "A short error",
  icon: "Security",
};

export const SecurityLongText = ({ ...args }): JSX.Element => (
  <ErrorBox text="" icon="Security" {...args} />
);

SecurityLongText.args = {
  text:
    "A very long test descbriing a very bad thing that happened and needs to be fixed",
  icon: "Security",
};
