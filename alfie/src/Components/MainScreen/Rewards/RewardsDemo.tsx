import React from "react";
import { Alert } from "react-native";
import { defaultTo, groupBy } from "lodash";

import useGetRewardList, { Reward } from "../../Hooks/UseGetRewardList";
import Rewards from "./Rewards";

const groupByBips = (data: Reward[]) =>
  groupBy(data, (x) => x.boostRewardInBips);

interface Props {
  gotoActivator: (level: string) => void;
}

export default function RewardsDemo(props: Props): JSX.Element {
  const allRewards = useGetRewardList();
  const groupedAll = groupByBips(allRewards.data);

  const transfer = () => {
    Alert.alert(
      "Not yet",
      "You'll need accrue at least $10 before you can transfer cash awards"
    );
  };

  return (
    <Rewards
      FiveRewards={defaultTo(groupedAll[500], [])}
      ThreeRewards={defaultTo(groupedAll[300], [])}
      selected={groupByBips([])}
      balance={36.87}
      entries={[]}
      gotoActivator={props.gotoActivator}
      transfer={transfer}
      transferInProgrss={false}
    />
  );
}
