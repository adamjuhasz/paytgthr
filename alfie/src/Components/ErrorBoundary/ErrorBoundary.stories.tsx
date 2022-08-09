import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";

import ErrorBoundary from "./ErrorBoundary";

export default {
  title: "Screens/ErrorBoundary",
  component: ErrorBoundary,
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof ErrorBoundary>> = ({
  ...args
}) => <ErrorBoundary {...args} />;
