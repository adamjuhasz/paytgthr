import React from "react";
import { useMutation, useQueryCache } from "react-query";
import { useSelector } from "react-redux";
import { Alert } from "react-native";

import { State } from "../../State/State";
import { acceptInvite } from "../../Actions/Invite/CodeBased";
import { queryPath } from "../Hooks/UseGetUser";

import AcceptInvite from "./AcceptInvite";

interface Props {
  goBack: () => void;
}

const AcceptInviteHOC = (props: Props): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const cache = useQueryCache();
  const [mutate, { isLoading }] = useMutation(acceptInvite(baseURL), {
    onSuccess: async () => {
      await cache.invalidateQueries([queryPath]);
      Alert.alert("Partnered up!");
      props.goBack();
    },
    onError: () => {
      Alert.alert("That didn't work, try again?");
    },
  });

  const sendCode = async (code: string) => {
    await mutate(code);
  };

  return (
    <AcceptInvite
      goBack={props.goBack}
      acceptInvite={sendCode}
      inProgress={isLoading}
    />
  );
};

export default AcceptInviteHOC;
