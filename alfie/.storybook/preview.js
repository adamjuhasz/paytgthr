// .storybook/preview.js

import React from "react";

import { INITIAL_VIEWPORTS } from "@storybook/addon-viewport";

export const parameters = {
  viewport: {
    defaultViewport: "iphonex",
    viewports: INITIAL_VIEWPORTS,
  },
};

export const decorators = [
  (Story) => {
    return <Story />;
  },
];
