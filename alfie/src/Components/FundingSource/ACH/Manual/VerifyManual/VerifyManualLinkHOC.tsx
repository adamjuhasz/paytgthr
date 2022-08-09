import React from "react";
import { useSelector } from "react-redux";
import { useQuery } from "react-query";
import { useHistory } from "../../../../../PlatformSpecific/react-router";

import { State } from "../../../../../State/State";
import { useSubmitAmount } from "../../../../../Actions/SignUp/BankAccount/SignUpVerifyManual";
import UserStateRouter from "../../../../Routers/UserStateRouter";
import {
  getCurrentUserState,
  queryPath,
} from "../../../../../Actions/User/GetUserState";
import VerifyManualLink from "./VerifyManualLink";

const VerifyManualLinkHOC = (): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const { inProgress, submit, errors, submitted } = useSubmitAmount(baseURL);
  const { data } = useQuery([queryPath], getCurrentUserState(baseURL));
  const history = useHistory();

  if (submitted) {
    return <UserStateRouter push={false} />;
  }

  if (data === undefined) {
    return (
      <VerifyManualLink
        submit={submit}
        inProgress={inProgress}
        {...errors}
        bankName={null}
        accountName={null}
        goBack={() => history.goBack()}
      />
    );
  } else {
    return (
      <VerifyManualLink
        submit={submit}
        inProgress={inProgress}
        {...errors}
        bankName={data.user.ach.bankName}
        accountName={data.user.ach.accountName}
        goBack={() => history.goBack()}
      />
    );
  }
};

export default VerifyManualLinkHOC;
