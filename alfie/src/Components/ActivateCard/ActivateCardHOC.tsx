import React from "react";
import { useMutation, useQueryCache } from "react-query";
import { useSelector } from "react-redux";
import { useParams } from "../../PlatformSpecific/react-router";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import { State } from "../../State/State";
import { activateCard } from "../../Actions/Card/Activate";
import { queryPath as cardQueryPath } from "../../Actions/Card/GetCards";
import ActivateCard from "./ActivateCard";

interface Props {
  goBack: () => void;
  goNext: (cardId: string) => void;
}

export default function PurchasesHOC(props: Props): JSX.Element {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const { cardid } = useParams<{
    cardid: string;
  }>();

  const [mutate, { isLoading }] = useMutation(activateCard(baseURL), {
    throwOnError: true,
  });

  const cache = useQueryCache();

  const activate = async (lastFour: string) => {
    void Analytics.track("Card Activate Attempt", { lastFour });
    await mutate({ cardId: cardid, lastFour: lastFour });
    await cache.invalidateQueries([cardQueryPath]);
    props.goNext(cardid);
  };

  return (
    <ActivateCard
      activateCard={activate}
      inProgress={isLoading}
      goBack={props.goBack}
    />
  );
}
