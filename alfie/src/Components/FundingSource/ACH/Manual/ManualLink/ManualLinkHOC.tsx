import React from "react";
import { useSelector } from "react-redux";
import { useHistory } from "../../../../../PlatformSpecific/react-router";

import { State } from "../../../../../State/State";
import { useSubmitManual } from "../../../../../Actions/SignUp/BankAccount/SignUpManualLink";
import ManualLink from "./ManualLink";

const bankRouting: {
  [key: string]: string;
} = require("../../../../../../assets/routingToBanks.json");

interface PropsHOC {
  gotoNext: () => void;
}

const ManualLinkHOC = (props: PropsHOC): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const { inProgress, submit, errors, submitted } = useSubmitManual(baseURL);
  const history = useHistory();

  if (submitted) {
    props.gotoNext();
    return <></>;
  }

  return (
    <ManualLink
      submit={submit}
      inProgress={inProgress}
      errors={errors}
      goBack={() => history.goBack()}
      routingList={bankRouting}
    />
  );
};

export default ManualLinkHOC;
