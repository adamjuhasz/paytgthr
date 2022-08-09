import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as boostsPath } from "./UseGetBoost";
import { path as activationsPaths } from "./UseGetBoostActivations";

export function useActivateBoost({
  groupid,
  userid,
}: {
  groupid: string;
  userid: string;
}): UseMutationResult<unknown, unknown, string, unknown> {
  const queryClient = useQueryClient();

  const mutation = useMutation(
    (rewardid: string) => {
      const form = new FormData();
      form.append("groupid", groupid);
      form.append("rewardid", rewardid);
      form.append("userid", userid);

      return axios.post(`${serverlocation}/rewards/activate`, form);
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([boostsPath, groupid]);
        await queryClient.invalidateQueries([activationsPaths, groupid]);
      },
    }
  );

  return mutation;
}
