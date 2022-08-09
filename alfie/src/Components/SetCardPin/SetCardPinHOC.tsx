import React from "react";
import { useMutation } from "react-query";
import { useSelector } from "react-redux";
import { useParams } from "../../PlatformSpecific/react-router";

import { State } from "../../State/State";
import { setCardPin } from "../../Actions/Card/SetPin";

import SetCardPin from "./SetCardPin";

interface Props {
  goBack: () => void;
  goNext: () => void;
}

export default function PurchasesHOC(props: Props): JSX.Element {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const { cardid } = useParams<{
    cardid: string;
  }>();

  const [mutate, { isLoading }] = useMutation(setCardPin(baseURL), {
    throwOnError: true,
  });

  const setPin = async (pin: string) => {
    await mutate({ cardId: cardid, pinCode: pin });
    props.goNext();
  };

  return (
    <SetCardPin setPin={setPin} inProgress={isLoading} goBack={props.goBack} />
  );
}
