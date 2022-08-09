import React from "react";
import { useMutation, useQuery } from "react-query";
import { useDispatch, useSelector } from "react-redux";

import { State } from "../../../State/State";
import { store } from "../../../State/Store";
import { changePhone } from "../../../Actions/SignUp/SignUpPhone";
import {
  getCurrentUserState,
  queryPath,
} from "../../../Actions/User/GetUserState";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import PhoneEntry from "./PhoneEntry";
import UserStateRouter from "../../Routers/UserStateRouter";

interface Props {
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const PhoneEntryHOC = (props: Props): JSX.Element => {
  const { baseURL, phone } = useSelector((state: State) => ({
    baseURL: state.baseURL,
    phone: state.userInfo.phone,
  }));

  const query = useQuery([queryPath], getCurrentUserState(baseURL), {
    staleTime: 100,
  });

  const dispatch = useDispatch<typeof store.dispatch>();

  const [mutate, { isLoading, isError, isSuccess }] = useMutation(
    changePhone(baseURL)
  );

  const changeTheEmail = async (newPhone: string) => {
    try {
      await mutate(newPhone);
      dispatch({ type: "changePhone", phone: newPhone });
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
    <PhoneEntry
      submit={changeTheEmail}
      inProgress={isLoading}
      phone={phoneNumber || ""}
      error={isError ? "Format" : null}
      trackEvent={Analytics.track}
      {...props}
    />
  );
};

export default PhoneEntryHOC;
