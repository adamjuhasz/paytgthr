import React from "react";
import { Alert } from "react-native";
import { useMutation, useQueryCache } from "react-query";
import { useSelector } from "react-redux";
import { defaultTo, groupBy } from "lodash";
import { useParams } from "../../../PlatformSpecific/react-router";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import useGetRewardList, { Reward } from "../../Hooks/UseGetRewardList";
import useGetOurRewards from "../../Hooks/UseGetOurRewards";
import { queryOurPath } from "../../../Actions/Rewards/GetRewards";
import BoostActivator from "./BoostActivator";
import { activateBoost } from "../../../Actions/Rewards/ActivateBoost";
import { State } from "../../../State/State";
import useGetUser from "../../Hooks/UseGetUser";

const groupByBips = (data: Reward[]) =>
  groupBy(data, (x) => x.boostRewardInBips);

interface Props {
  goNext: () => void;
  goBack: () => void;
}

export default function BoostActivatorHOC(props: Props): JSX.Element {
  const { level } = useParams<{
    level: string;
  }>();
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const user = useGetUser();

  const queryCache = useQueryCache();
  const [mutate, { isLoading, isSuccess }] = useMutation(
    activateBoost(baseURL),
    {
      throwOnError: true,
      onSuccess: async () => {
        await queryCache.invalidateQueries([queryOurPath]);
      },
    }
  );

  const activate = async (boostId: string) => {
    if (user === null) {
      Alert.alert(
        "Can't set boost",
        "You'll need to partner up before you can set a couple's boost"
      );
      return;
    }

    if (user !== null && user.group.status !== "groupactive") {
      void Analytics.track("User RewardBoost FailNoGroup", { boost: boostId });
      Alert.alert(
        "Can't set boost",
        "You'll need to partner up before you can set a couple's boost"
      );
      return;
    }

    await mutate(boostId);
    props.goNext();
  };

  const allRewards = useGetRewardList();
  const ourRewards = useGetOurRewards();

  const groupedAll = groupByBips(allRewards.data);

  return (
    <BoostActivator
      rewards={defaultTo(groupedAll[level], [])}
      level={level}
      selected={defaultTo(ourRewards.data, [])}
      activateBoost={activate}
      goBack={props.goBack}
      inProgress={isLoading || isSuccess}
    />
  );
}
