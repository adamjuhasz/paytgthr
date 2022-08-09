import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import { useHistory } from "../../PlatformSpecific/react-router";

import { useRequestToken } from "../../Actions/User/RequestToken";
import { State } from "../../State/State";

import PasswordCodeSend, { Mediums } from "./PasswordCodeSend";

interface Props {
  medium: Mediums;
  gotoReset: (medium: Mediums, iden: string) => void;
}

const PasswordCodeSendHOC = (props: Props): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const [info, setInfo] = useState<null | [Mediums, string]>(null);
  const history = useHistory();
  const { submit, state } = useRequestToken(baseURL);

  const generateToken = (medium: Mediums, iden: string) => {
    void Analytics.track("Password Reset Token Requested", { medium });
    void submit(medium, iden)
      .then(() => {
        setInfo([medium, iden]);
      })
      .catch(() => {
        // don't leak existing users
        void Analytics.track("Password Reset Identifier Incorrect", { medium });
        setInfo([medium, iden]);
      });
  };

  useEffect(() => {
    if ((state === "submited" || state === "error") && info !== null) {
      props.gotoReset(info[0], info[1]);
    }
  }, [state, info, props]);

  switch (state) {
    case "none":
    case "error":
      return (
        <PasswordCodeSend
          medium={props.medium}
          submit={generateToken}
          inProgress={false}
          goBack={() => history.goBack()}
        />
      );

    case "inProgress":
    case "submited":
      return (
        <PasswordCodeSend
          medium={props.medium}
          submit={generateToken}
          inProgress={true}
          goBack={() => history.goBack()}
        />
      );
  }
};

export default PasswordCodeSendHOC;
