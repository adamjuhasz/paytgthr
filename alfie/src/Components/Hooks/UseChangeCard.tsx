import { MutationResultPair, useMutation, useQueryCache } from "react-query";
import { useSelector } from "react-redux";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import Alert from "../../PlatformSpecific/Alert";

import { lockCard, unlockCard } from "../../Actions/Card/SetCardState";
import { State } from "../../State/State";
import { path as cardQueryPath } from "./UseGetCards";

export function useLockCard(): MutationResultPair<
  void,
  unknown,
  string,
  unknown
> {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const cache = useQueryCache();

  const mutate = useMutation(lockCard(baseURL), {
    onMutate: () => {
      Alert.alert(
        "You card is now locked",
        "This did not not lock your partner's card"
      );
    },
    onSuccess: async () => {
      void Analytics.track("User Card Locked");
      await cache.invalidateQueries([cardQueryPath]);
      await cache.refetchQueries([cardQueryPath], { stale: true });
    },
  });

  return mutate;
}

export function useUnlockCard(): MutationResultPair<
  void,
  unknown,
  string,
  unknown
> {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const cache = useQueryCache();

  const mutate = useMutation(unlockCard(baseURL), {
    onMutate: () => {
      Alert.alert("You card is now unlocked");
    },
    onSuccess: async () => {
      await cache.invalidateQueries([cardQueryPath]);
      await cache.refetchQueries([cardQueryPath], { stale: true });
      void Analytics.track("User Card Unlocked");
    },
  });

  return mutate;
}
