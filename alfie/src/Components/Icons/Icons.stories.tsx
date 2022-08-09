import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";

import GroupIcon from "./Group";
import LockedIcon from "./Locked";
import UnlockedIcon from "./Unlocked";
import NumberPadIcon from "./NumberPad";
import PaperAirplaneIcon from "./PaperAirplane";
import EnvelopeIcon from "./Envelope";
import PieChartIcon from "./PieChart";
import GearIcon from "./Gear";
import BankIcon from "./Bank";
import CreditCardIcon from "./CreditCard";
import RingingBellICon from "./RingingBell";
import BaBarGraphIcon from "./BarGraph";
import UserIcon from "./User";
import StarIcon from "./Star";
import NoteIcon from "./Note";
import TrafficConeIcon from "./TrafficCone";
import LinkIcon from "./Link";
import GlobeIcon from "./Globe";
import IdIcon from "./Id";
import MoneyIcon from "./Money";
import ClockIcon from "./Clock";
import DiamondIcon from "./Diamond";
import LightningBoltIcon from "./LightningBolt";
import BugIcon from "./Bug";
import MarketplaceIcon from "./Marketplace";
import TrendingArrowIcon from "./TrendingArrow";
import ExportIcon from "./Export";
import InformationIcon from "./Information";
import CheckmarkIcon from "./Checkmark";
import DeleteIcon from "./Delete";
import LeftChevronIcon from "./LeftChevron";
import RightChevronIcon from "./RightChevron";
import ShieldIcon from "./Shield";
import ExclamationPointIcon from "./ExclamationPoint";

export default {
  title: "Components/Icons",
  argTypes: {
    color: {
      defaultValue: "#EA4352",
      control: {
        type: "color",
      },
    },
    size: {
      defaultValue: 40,
      control: {
        type: "range",
        min: 10,
        max: 200,
        step: 10,
      },
    },
  },
} as Meta;

export const Group: Story<React.ComponentProps<typeof GroupIcon>> = ({
  ...args
}): JSX.Element => <GroupIcon {...args} />;

export const Locked: Story<React.ComponentProps<typeof LockedIcon>> = ({
  ...args
}): JSX.Element => <LockedIcon {...args} />;

export const Unlocked: Story<React.ComponentProps<typeof UnlockedIcon>> = ({
  ...args
}): JSX.Element => <UnlockedIcon {...args} />;

export const NumberPad: Story<React.ComponentProps<typeof NumberPadIcon>> = ({
  ...args
}): JSX.Element => <NumberPadIcon {...args} />;

export const PaperAirplane: Story<
  React.ComponentProps<typeof PaperAirplaneIcon>
> = ({ ...args }): JSX.Element => <PaperAirplaneIcon {...args} />;

export const Envelope: Story<React.ComponentProps<typeof EnvelopeIcon>> = ({
  ...args
}): JSX.Element => <EnvelopeIcon {...args} />;

export const PieChart: Story<React.ComponentProps<typeof PieChartIcon>> = ({
  ...args
}): JSX.Element => <PieChartIcon {...args} />;

export const Gear: Story<React.ComponentProps<typeof GearIcon>> = ({
  ...args
}): JSX.Element => <GearIcon {...args} />;

export const Bank: Story<React.ComponentProps<typeof BankIcon>> = ({
  ...args
}): JSX.Element => <BankIcon {...args} />;

export const CreditCard: Story<React.ComponentProps<typeof CreditCardIcon>> = ({
  ...args
}): JSX.Element => <CreditCardIcon {...args} />;

export const RingingBell: Story<
  React.ComponentProps<typeof RingingBellICon>
> = ({ ...args }): JSX.Element => <RingingBellICon {...args} />;

export const BarGraph: Story<React.ComponentProps<typeof BaBarGraphIcon>> = ({
  ...args
}): JSX.Element => <BaBarGraphIcon {...args} />;

export const User: Story<React.ComponentProps<typeof UserIcon>> = ({
  ...args
}): JSX.Element => <UserIcon {...args} />;

export const Star: Story<React.ComponentProps<typeof StarIcon>> = ({
  ...args
}): JSX.Element => <StarIcon {...args} />;

export const Note: Story<React.ComponentProps<typeof NoteIcon>> = ({
  ...args
}): JSX.Element => <NoteIcon {...args} />;

export const TrafficCone: Story<
  React.ComponentProps<typeof TrafficConeIcon>
> = ({ ...args }): JSX.Element => <TrafficConeIcon {...args} />;

export const Link: Story<React.ComponentProps<typeof LinkIcon>> = ({
  ...args
}): JSX.Element => <LinkIcon {...args} />;

export const Globe: Story<React.ComponentProps<typeof GlobeIcon>> = ({
  ...args
}): JSX.Element => <GlobeIcon {...args} />;

export const Id: Story<React.ComponentProps<typeof IdIcon>> = ({
  ...args
}): JSX.Element => <IdIcon {...args} />;

export const Money: Story<React.ComponentProps<typeof MoneyIcon>> = ({
  ...args
}): JSX.Element => <MoneyIcon {...args} />;

export const Clock: Story<React.ComponentProps<typeof ClockIcon>> = ({
  ...args
}): JSX.Element => <ClockIcon {...args} />;

export const Diamond: Story<React.ComponentProps<typeof DiamondIcon>> = ({
  ...args
}): JSX.Element => <DiamondIcon {...args} />;

export const LightningBolt: Story<
  React.ComponentProps<typeof LightningBoltIcon>
> = ({ ...args }): JSX.Element => <LightningBoltIcon {...args} />;

export const Bug: Story<React.ComponentProps<typeof BugIcon>> = ({
  ...args
}): JSX.Element => <BugIcon {...args} />;

export const Marketplace: Story<
  React.ComponentProps<typeof MarketplaceIcon>
> = ({ ...args }): JSX.Element => <MarketplaceIcon {...args} />;

export const TrendingArrow: Story<
  React.ComponentProps<typeof TrendingArrowIcon>
> = ({ ...args }): JSX.Element => <TrendingArrowIcon {...args} />;

export const Export: Story<React.ComponentProps<typeof ExportIcon>> = ({
  ...args
}): JSX.Element => <ExportIcon {...args} />;

export const Information: Story<
  React.ComponentProps<typeof InformationIcon>
> = ({ ...args }): JSX.Element => <InformationIcon {...args} />;

export const Checkmark: Story<React.ComponentProps<typeof CheckmarkIcon>> = ({
  ...args
}): JSX.Element => <CheckmarkIcon {...args} />;

export const Delete: Story<React.ComponentProps<typeof DeleteIcon>> = ({
  ...args
}): JSX.Element => <DeleteIcon {...args} />;

export const LeftChevron: Story<
  React.ComponentProps<typeof LeftChevronIcon>
> = ({ ...args }): JSX.Element => <LeftChevronIcon {...args} />;

export const RightChevron: Story<
  React.ComponentProps<typeof RightChevronIcon>
> = ({ ...args }): JSX.Element => <RightChevronIcon {...args} />;

export const Shield: Story<React.ComponentProps<typeof ShieldIcon>> = ({
  ...args
}): JSX.Element => <ShieldIcon {...args} />;

export const ExclamationPoint: Story<
  React.ComponentProps<typeof ExclamationPointIcon>
> = ({ ...args }): JSX.Element => <ExclamationPointIcon {...args} />;
