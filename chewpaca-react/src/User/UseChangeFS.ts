import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as modelPath } from "./UseGetUserModel";
import { path as paymentPath } from "../Payments/UseGetUserPayments";

export function useResendFSVerification(
  user: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/fundingsource/verification/resend".replace(
    ":uid",
    user
  );

  const mutation = useMutation(
    () => {
      const form = new FormData();
      return axios.post(`${serverlocation}${realpath}`, form);
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([paymentPath, user]);
      },
    }
  );

  return mutation;
}

export function useManualFSVerify(
  user: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/fundingsource/verify".replace(":uid", user);

  const mutation = useMutation(
    () => {
      const form = new FormData();
      return axios.post(`${serverlocation}${realpath}`, form);
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([modelPath, user]);
      },
    }
  );

  return mutation;
}

export function useRemoveFS(
  user: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/fundingsource/remove".replace(":uid", user);

  const mutation = useMutation(
    () => {
      const form = new FormData();
      return axios.post(`${serverlocation}${realpath}`, form);
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([modelPath, user]);
      },
    }
  );

  return mutation;
}
