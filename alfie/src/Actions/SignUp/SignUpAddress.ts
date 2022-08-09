import { useState } from "react";
import { queryCache } from "react-query";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import console from "../../Global/Console";

import { useAsync } from "../cancellablePromise";
import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { UserModel } from "../GetUserType";
import { Address as AddressModel } from "../AddressVerification";
import { formdataHeaders } from "../../Helpers/FormData";

export type Address = AddressModel;

interface AddressResponse {
  "street-error"?: boolean;
  "city-error"?: boolean;
  "state-error"?: boolean;
  "zip-error"?: boolean;
  user: {
    email: string;
    firstName: string;
    lastName: string;
    userid: string;
  };
  "street-value"?: string;
  "street2-value"?: string;
  "city-value"?: string;
  "state-value"?: string;
  "zip-value"?: string;
}

export const getAddress = (
  baseURL: string
) => async (): Promise<AddressModel> => {
  const url = `${baseURL}/app/signup/address`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as UserModel;
      const converted: Address = {
        street: body["street-value"] === null ? "" : body["street-value"],
        apt: body["street2-value"] === null ? "" : body["street2-value"],
        city: body["city-value"] === null ? "" : body["city-value"],
        state: body["state-value"] === null ? "" : body["state-value"],
        zip: body["zip-value"] === null ? "" : body["zip-value"],
      };

      return converted;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`submitAddress had return of ${res.status}`);
  }
};

export const submitAddress = (baseURL: string) => async (
  address: Address
): Promise<void> => {
  const { street, apt, city, state, zip } = address;

  const formdata = new FormData();
  formdata.append("street", street);
  formdata.append("street2", apt);
  formdata.append("city", city);
  formdata.append("state", state);
  formdata.append("zip", zip);

  await Analytics.track("User AddressChange Attempt", { ...address });

  const url = `${baseURL}/app/signup/address`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, ...formdataHeaders(formdata) },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Address Entered", { ...address });
      await Analytics.setTrait("city", city);
      await Analytics.setTrait("country", "USA");
      await Analytics.setTrait("state", state);
      await Analytics.setTrait("zip", zip);
      return;

    case 400: {
      const body = (await res.json()) as AddressResponse;
      const errors: AddressErrors = {
        street: "None",
        city: "None",
        state: "None",
        zip: "None",
      };
      if (body["street-error"] === true) {
        errors.street = "Format";
      }
      if (body["city-error"] === true) {
        errors.city = "Format";
      }
      if (body["state-error"] === true) {
        errors.state = "Format";
      }
      if (body["zip-error"] === true) {
        errors.zip = "Format";
      }

      await Analytics.track("Address Entered Error", {
        httpStatus: res.status,
        httpUrl: url,
        ...errors,
      });

      throw errors;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { ...address },
      });
      throw new Error(`submitAddress had return of ${res.status}`);
  }
};

type StreetError = "None" | "Format";
type CityError = "None" | "Format";
type StateError = "None" | "Format";
type ZipError = "None" | "Format";
export interface AddressErrors {
  street: StreetError;
  city: CityError;
  state: StateError;
  zip: ZipError;
}

type Return = {
  inProgress: boolean;
  submitted: boolean;
  submit: (address: Address) => void;
  errors: AddressErrors;
};
export const useSubmitAddress = (baseURL: string): Return => {
  const initialErrors: AddressErrors = {
    street: "None",
    city: "None",
    state: "None",
    zip: "None",
  };
  const [inProgress, execute, currentErrors] = useAsync(initialErrors);
  const [success, setSuccess] = useState(false);

  const submit = (address: Address) => {
    execute(submitAddress(baseURL)(address))
      .then(() => {
        setSuccess(true);
        queryCache.removeQueries(["/app/signup/address"]);
        queryCache.removeQueries(["/app/signup/kyc"]);
      })
      .catch((err) => {
        console.error("error in useSubmitAddress", err);
      });
  };

  return { inProgress, submit, errors: currentErrors, submitted: success };
};
