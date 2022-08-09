/* eslint-disable react/prop-types */
/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import {} from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import FormScreen from "./FormScreen";
import Button from "../Button/Button";
import { ShrugIcon } from "../PopUp";

export default {
  title: "Components/FormScreen",
  component: FormScreen,
  argTypes: {
    goBack: { action: "navigation" },
    logout: { action: "logout" },
    primaryOnPress: { action: "Primary Button" },
    secondaryOnPress: { action: "Secondary Button" },
    navAction: { action: "Navigation" },
    buttons: {
      control: {
        type: null,
      },
    },
    background: {
      control: {
        type: null,
      },
    },
    navigation: {
      control: {
        type: null,
      },
    },
    textColor: {
      control: {
        type: "color",
      },
    },
    rightIcon: {
      control: {
        type: null,
      },
    },
  },
} as Meta;

type Args = React.ComponentProps<typeof FormScreen> & {
  primaryOnPress: () => void;
  secondaryOnPress: () => void;
  navAction: () => void;
};

export const NoNav: Story<Args> = ({
  primaryOnPress,
  secondaryOnPress,
  ...args
}): JSX.Element => {
  return (
    <ThemeProvider>
      <FormScreen
        buttons={
          <>
            <Button
              text="Primary action"
              style="Primary"
              onPress={primaryOnPress}
            />
            <Button
              text="Secondary action"
              style="Secondary"
              onPress={secondaryOnPress}
            />
          </>
        }
        {...args}
      />
    </ThemeProvider>
  );
};

NoNav.args = {
  title: "Page title",
  navigation: {
    type: "none",
  },
};

export const ActionNav: Story<Args> = ({
  primaryOnPress,
  secondaryOnPress,
  navAction,
  ...args
}): JSX.Element => {
  return (
    <ThemeProvider>
      <FormScreen
        buttons={
          <>
            <Button
              text="Primary action"
              style="Primary"
              onPress={primaryOnPress}
            />
            <Button
              text="Secondary action"
              style="Secondary"
              onPress={secondaryOnPress}
            />
          </>
        }
        {...args}
        navigation={{ type: "action", action: navAction }}
      />
    </ThemeProvider>
  );
};

ActionNav.args = {
  title: "Page title",
};

export const RightAction: Story<Args> = ({
  primaryOnPress,
  secondaryOnPress,
  navAction,
  ...args
}): JSX.Element => {
  return (
    <ThemeProvider>
      <FormScreen
        buttons={
          <>
            <Button
              text="Primary action"
              style="Primary"
              onPress={primaryOnPress}
            />
            <Button
              text="Secondary action"
              style="Secondary"
              onPress={secondaryOnPress}
            />
          </>
        }
        {...args}
        navigation={{ type: "action", action: navAction }}
        rightIcon={<ShrugIcon />}
      />
    </ThemeProvider>
  );
};

RightAction.args = {
  title: "Page title",
};
