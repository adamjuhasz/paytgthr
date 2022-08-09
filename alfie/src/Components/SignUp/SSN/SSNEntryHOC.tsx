import React, { useEffect } from "react";
import { useMutation } from "react-query";
import { useSelector } from "react-redux";

import { State } from "../../../State/State";
import { changeSSN } from "../../../Actions/SignUp/SignUpSSN";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import SSNEntry, { Errors } from "./SSNEntry";

interface Props {
  gotoNextScreen: () => void;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const SSNEntryHOC = (props: Props): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const [mutate, { isLoading, isError, data }] = useMutation(
    changeSSN(baseURL)
  );

  useEffect(() => {
    if (
      data !== undefined &&
      data["ssn-error"] === false &&
      data["ssn-reuse-error"] === false
    ) {
      props.gotoNextScreen();
    }
  }, [data, props]);

  const changeTheSSN = async (newSSN: string) => {
    try {
      await mutate(newSSN);
    } catch (e) {
      return;
    }
  };

  let error: Errors = null;
  if (isError) {
    error = "Format";
  } else if (data?.["ssn-error"]) {
    error = "Format";
  } else if (data?.["ssn-reuse-error"]) {
    error = "ReUse";
  }

  return (
    <SSNEntry
      submit={changeTheSSN}
      inProgress={isLoading}
      ssn=""
      {...props}
      error={error}
      trackEvent={Analytics.track}
    />
  );
};

export default SSNEntryHOC;
