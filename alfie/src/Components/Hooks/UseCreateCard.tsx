import { MutationResultPair, useMutation, useQueryCache } from "react-query";
import { useSelector } from "react-redux";
import Alert from "../../PlatformSpecific/Alert";

import { Inputs, createCard } from "../../Actions/Card/CreateCard";
import { State } from "../../State/State";
import { path as cardQueryPath } from "./UseGetCards";

export function useCreateCard(): MutationResultPair<
  void,
  unknown,
  Inputs,
  unknown
> {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const cache = useQueryCache();

  const mutate = useMutation(createCard(baseURL), {
    onMutate: () => {
      Alert.alert("You card is on its way");
    },
    onSuccess: async () => {
      await cache.invalidateQueries([cardQueryPath]);
      await cache.refetchQueries([cardQueryPath], { stale: true });
    },
  });

  return mutate;
}
