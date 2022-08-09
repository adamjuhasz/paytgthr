/* eslint-disable @typescript-eslint/no-explicit-any */
import React from "react";
import { Text } from "react-native";
import { Meta } from "@storybook/react/types-6-0";

import TextStyles from "./Text";

export default {
  title: "Styles/Text",
  argTypes: {
    value: {
      type: { name: "string", required: false },
      control: {
        type: "text",
      },
      defaultValue: "The quick brown fox jumps over the lazy dog",
    },
  },
} as Meta;

export const FontWeight400 = ({ value }: Record<string, any>): JSX.Element => (
  <Text style={[TextStyles.fontWeight400]}>{value}</Text>
);

export const FontWeight500 = ({ value }: Record<string, any>): JSX.Element => (
  <Text style={[TextStyles.fontWeight500]}>{value}</Text>
);

export const FontWeight600 = ({ value }: Record<string, any>): JSX.Element => (
  <Text style={[TextStyles.fontWeight600]}>{value}</Text>
);

export const NexaLight = ({ value }: Record<string, any>): JSX.Element => (
  <Text style={[TextStyles.nexaLight]}>{value}</Text>
);

export const NexaHeavy = ({ value }: Record<string, any>): JSX.Element => (
  <Text style={[TextStyles.nexaHeavy]}>{value}</Text>
);

export const NormalText = ({ value }: Record<string, any>): JSX.Element => (
  <Text style={[TextStyles.fontWeight400, TextStyles.normalText]}>
    {value}{" "}
  </Text>
);
