import React from "react";
import { useMutation } from "react-query";
import { useSelector } from "react-redux";

import { State } from "../../../State/State";
import { changeDOB } from "../../../Actions/SignUp/SignUpDOB";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import DOBEntry from "./DOBEntry";
import UserStateRouter from "../../Routers/UserStateRouter";

interface Props {
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const DOBEntryHOC = (props: Props): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const [mutate, { isLoading, isError, isSuccess }] = useMutation(
    changeDOB(baseURL)
  );

  const changeTheDOB = async (newDOB: string) => {
    try {
      await mutate(newDOB);
    } catch (e) {
      return;
    }
  };

  if (isSuccess) {
    return <UserStateRouter push />;
  }

  return (
    <DOBEntry
      submit={changeTheDOB}
      inProgress={isLoading}
      error={isError ? "Format" : null}
      usersName={"people with the same name"}
      dob=""
      trackEvent={Analytics.track}
      {...props}
    />
  );
};

export default DOBEntryHOC;
