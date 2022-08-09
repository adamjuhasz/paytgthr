import React from "react";
import { Alert } from "react-native";
import { defaultTo, groupBy } from "lodash";
import { useMutation, useQueryCache } from "react-query";
import { useSelector } from "react-redux";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import { State } from "../../../State/State";
import useGetRewardList, { Reward } from "../../Hooks/UseGetRewardList";
import useGetOurRewards from "../../Hooks/UseGetOurRewards";
import Rewards from "./Rewards";
import useGetRewardEntries from "../../Hooks/UseGetRewardEntries";
import { transferReward } from "../../../Actions/Rewards/TransferRewards";
import { queryEntries } from "../../../Actions/Rewards/GetRewards";

const groupByBips = (data: Reward[]) =>
  groupBy(data, (x) => x.boostRewardInBips);

interface Props {
  gotoActivator: (level: string) => void;
}

export default function RewardsHOC(props: Props): JSX.Element {
  const allRewards = useGetRewardList();
  const ourRewards = useGetOurRewards();
  const entries = useGetRewardEntries();
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const queryCache = useQueryCache();

  const [mutate, { isLoading }] = useMutation(transferReward(baseURL), {
    throwOnError: true,
    onSuccess: async () => {
      await queryCache.invalidateQueries([queryEntries]);
    },
  });

  const groupedAll = groupByBips(allRewards.data);
  let balance = Infinity;
  if (entries.status === "success") {
    balance = entries.data.balance[1] / entries.data.balance[2];
  }

  const transfer = (amount: number) => {
    if (balance < 1 || balance === Infinity) {
      void Analytics.track("User RewardsBalance TransferFailed", { amount });
      Alert.alert(
        "Not yet",
        "You'll need accrue at least $1 before you can transfer cash awards"
      );
      return;
    }

    const formatedAmount = amount.toFixed(2);

    Alert.alert(
      "Ready?",
      `This will transfer $${formatedAmount} to your current Tgthr Card balance`,
      [
        {
          text: "Ok",
          style: "default",
          onPress: async () => {
            void Analytics.track("User RewardsBalance Transfer", { amount });
            await mutate(amount);
          },
        },
        {
          text: "Cancel",
          style: "cancel",
          onPress: () => {
            void Analytics.track("User RewardsBalance Cancel", { amount });
            return;
          },
        },
      ],
      { cancelable: true }
    );
  };

  return (
    <Rewards
      FiveRewards={defaultTo(groupedAll[500], [])}
      ThreeRewards={defaultTo(groupedAll[300], [])}
      selected={groupByBips(defaultTo(ourRewards.data, []))}
      balance={balance}
      entries={defaultTo(entries.data?.entries, []).filter(
        (r) => r.amount[1] !== 0
      )}
      gotoActivator={props.gotoActivator}
      transfer={transfer}
      transferInProgrss={isLoading}
    />
  );
}
