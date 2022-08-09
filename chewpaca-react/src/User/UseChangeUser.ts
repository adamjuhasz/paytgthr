import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as modelPath } from "./UseGetUserModel";

export function useCreatePrivacy(
  user: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/privacy/sendto".replace(":uid", user);

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

export function useCreateDwolla(
  user: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/dwolla/sendto".replace(":uid", user);

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

interface EmailChange {
  email: string;
}

export function useChangeEmail(
  user: string
): UseMutationResult<unknown, unknown, EmailChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/email".replace(":uid", user);

  const mutation = useMutation(
    ({ email }: EmailChange) => {
      const form = new FormData();
      form.append("email", email);
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

interface AddressChange {
  street: string;
  city: string;
  state: string;
  zip: string;
}

export function useChangeAddress(
  user: string
): UseMutationResult<unknown, unknown, AddressChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/address".replace(":uid", user);

  const mutation = useMutation(
    (addr: AddressChange) => {
      const form = new FormData();
      form.append("firstline", addr.street);
      form.append("secondline", "");
      form.append("city", addr.city);
      form.append("state", addr.state);
      form.append("zip", addr.zip);
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

interface PhoneChange {
  phone: string;
}

export function useChangePhone(
  user: string
): UseMutationResult<unknown, unknown, PhoneChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/phone".replace(":uid", user);

  const mutation = useMutation(
    ({ phone }: PhoneChange) => {
      const form = new FormData();
      form.append("phone", phone);
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

interface SSNChange {
  ssn: string;
}

export function useChangeSSN(
  user: string
): UseMutationResult<unknown, unknown, SSNChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/ssn".replace(":uid", user);

  const mutation = useMutation(
    ({ ssn }: SSNChange) => {
      const form = new FormData();
      form.append("ssn", ssn);
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

interface NameChange {
  first: string;
  last: string;
}

export function useChangeName(
  user: string
): UseMutationResult<unknown, unknown, NameChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/name".replace(":uid", user);

  const mutation = useMutation(
    ({ first, last }: NameChange) => {
      const form = new FormData();
      form.append("fname", first);
      form.append("lname", last);
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

interface DwollaChange {
  dwollaid: string;
  dwollafsid: string;
}

export function useChangeDwolla(
  user: string
): UseMutationResult<unknown, unknown, DwollaChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/dwolla".replace(":uid", user);

  const mutation = useMutation(
    ({ dwollaid, dwollafsid }: DwollaChange) => {
      const form = new FormData();
      form.append("dwollaid", dwollaid);
      form.append("dwollafsid", dwollafsid);
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

interface BankChange {
  bankname: string;
  accountname: string;
  routingnumber: string;
  accountnumber: string;
}

export function useChangeBank(
  user: string
): UseMutationResult<unknown, unknown, BankChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/fundingsource/bank".replace(":uid", user);

  const mutation = useMutation(
    (bank: BankChange) => {
      const form = new FormData();
      form.append("bankname", bank.bankname);
      form.append("accountname", bank.accountname);
      form.append("routingnumber", bank.routingnumber);
      form.append("accountnumber", bank.accountnumber);
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

type CloseReasons = string;
export function useCloseUser(
  user: string
): UseMutationResult<unknown, unknown, CloseReasons, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/state/close/:reason".replace(":uid", user);

  const mutation = useMutation(
    (reason: CloseReasons) => {
      const pathWithReason = realpath.replace(":reason", reason);
      return axios.post(`${serverlocation}${pathWithReason}`);
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([modelPath, user]);
      },
    }
  );

  return mutation;
}

interface DOBChange {
  dob: string;
}

export function useChangeDOB(
  user: string
): UseMutationResult<unknown, unknown, DOBChange, unknown> {
  const queryClient = useQueryClient();

  const realpath = "/user/:uid/change/dob".replace(":uid", user);

  const mutation = useMutation(
    ({ dob }: DOBChange) => {
      const form = new FormData();
      form.append("dob", dob);
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
