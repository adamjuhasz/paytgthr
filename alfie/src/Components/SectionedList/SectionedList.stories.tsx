/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import FullScreenAutoBackground from "../Theming/AutoBackground";

import GroupIcon from "../Icons/Group";
import PieChart from "../Icons/PieChart";
import BankIcon from "../Icons/Bank";
import Locked from "../Icons/Locked";
import PaperAirplane from "../Icons/PaperAirplane";
import Envelope from "../Icons/Envelope";
import NumberPad from "../Icons/NumberPad";
import Diamond from "../Icons/Diamond";
import TrafficCone from "../Icons/TrafficCone";

import SectionedList from "./SectionedList";

export default {
  title: "Components/SectionedList",
  component: SectionedList,
} as Meta;

type Args = React.ComponentProps<typeof SectionedList>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <FullScreenAutoBackground>
      <SectionedList {...args} />
    </FullScreenAutoBackground>
  </ThemeProvider>
);
BaseState.args = {
  data: [
    {
      sectionType: "text",
      title: "Account settings",
      data: [
        {
          type: "text",
          text: "Change group",
          icon: GroupIcon,
          action: () => 1,
        },
        {
          type: "text",
          text: "Change current split",
          icon: PieChart,
          action: () => 1,
        },
        {
          type: "text",
          text: "Change bank account",
          icon: BankIcon,
          action: () => 1,
        },
        { type: "text", text: "Lock card", icon: Locked, action: () => 1 },
      ],
    },
    {
      sectionType: "text",
      title: "Get help",
      data: [
        {
          type: "text",
          text: "Text support",
          icon: PaperAirplane,
          action: () => 1,
        },
        {
          type: "text",
          text: "Email support",
          icon: Envelope,
          action: () => 1,
        },
        {
          type: "text",
          text: "Call support",
          icon: NumberPad,
          action: () => 1,
        },
      ],
    },
    {
      sectionType: "text",
      title: "Earn something",
      data: [
        {
          type: "text",
          text: "Refer a friend",
          icon: Diamond,
          helpText:
            "Earn $40 when you refer a friend to get a Tgthr Card with their partner ",
          action: () => 1,
        },
        {
          type: "text",
          text: "Liz Boston",
          progress: { value: 20, color: "#EA4352" },
        },
        {
          type: "text",
          text: "Jackson Davis",
          progress: { value: 110, color: "#A069FF" },
        },
        {
          type: "text",
          text: "Brock Johson",
          progress: { value: -10, color: "#65DBA2" },
        },
      ],
    },
    {
      sectionType: "text",
      title: "Logout",
      data: [
        { type: "text", text: "Logout", icon: TrafficCone, action: () => 1 },
      ],
    },
  ],
};
