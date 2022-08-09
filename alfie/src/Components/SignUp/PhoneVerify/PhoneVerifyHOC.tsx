import React, { useEffect } from "react";
import { useMutation, useQuery } from "react-query";
import { useSelector } from "react-redux";
import { Alert } from "react-native";

import { State } from "../../../State/State";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import { verifyPhone } from "../../../Actions/SignUp/SignUpPhone";
import { requestToken } from "../../../Actions/User/RequestToken";
import {
  getCurrentUserState,
  queryPath,
} from "../../../Actions/User/GetUserState";
import PhoneVerify from "./PhoneVerify";
import UserStateRouter from "../../Routers/UserStateRouter";

interface Props {
  gotoChangePhone: () => void;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const PhoneVerifyHOC = (props: Props): JSX.Element => {
  const { baseURL, phone } = useSelector((state: State) => ({
    baseURL: state.baseURL,
    phone: state.userInfo.phone,
  }));
  const [mutate, { isLoading, isError, isSuccess }] = useMutation(
    verifyPhone(baseURL)
  );
  const [getToken, { isError: tokenError }] = useMutation(
    requestToken(baseURL)
  );

  const query = useQuery([queryPath], getCurrentUserState(baseURL), {
    staleTime: 100,
  });

  useEffect(() => {
    if (tokenError) {
      Alert.alert("Please contact support about your phone number");
    }
  }, [tokenError]);

  const sendCodeToUser = async () => {
    if (query.data !== undefined && query.isStale === false) {
      const number = query.data.user.phone.number || "";
      await getToken({ medium: "sms", identifier: number.replace(/\D/g, "") });
    } else if (phone !== undefined) {
      await getToken({ medium: "sms", identifier: phone.replace(/\D/g, "") });
    } else {
      Alert.alert("Please contact support about your phone number");
    }
  };

  const sendToken = async (code: string) => {
    try {
      await mutate(code);
    } catch (e) {
      return;
    }
  };

  const phoneNumber =
    query.data !== undefined ? query.data.user.phone.number : phone;

  if (isSuccess) {
    return <UserStateRouter push />;
  }

  return (
    <PhoneVerify
      submit={sendToken}
      inProgress={isLoading}
      {...props}
      phone={phoneNumber || ""}
      error={isError}
      sendCode={sendCodeToUser}
      trackEvent={Analytics.track}
    />
  );
};

export default PhoneVerifyHOC;
