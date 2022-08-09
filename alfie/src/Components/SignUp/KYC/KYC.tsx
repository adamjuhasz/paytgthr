import React, { useEffect } from "react";
import { useQuery } from "react-query";
import { useDispatch, useSelector } from "react-redux";
import { Redirect } from "../../../PlatformSpecific/react-router";
import { ActivityIndicator } from "react-native";
import { useHistory } from "../../../PlatformSpecific/react-router";
import console from "../../../Global/Console";

import { SettingsPaths } from "../../Routers/SettingsRouter/Paths";
import { AlfieDispatch } from "../../../State/Store";
import { State } from "../../../State/State";
import { getKYCState } from "../../../Actions/SignUp/SignUpKYC";
import FormScreen from "../../Form/FormScreen";
import { registerForPushNotificationsAsync } from "../../../PlatformSpecific/PushNotifcations";

import KYCDelay from "./Delay";
import KYCManual from "./Manual";
import KYCFailed from "./Failed";
import KYCProcessing from "./Processing";
import KYCApproved from "./Approved";
export { KYCDelay, KYCManual, KYCFailed, KYCProcessing };
import { logout } from "../../../Actions/Logout";

const KYCState = (): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const dispatch = useDispatch<AlfieDispatch>();

  const query = useQuery(["/app/signup/kyc"], getKYCState(baseURL), {
    staleTime: 0,
  });
  const history = useHistory();

  useEffect(() => {
    void registerForPushNotificationsAsync();
  }, []);

  useEffect(() => {
    const logoutUser = () => dispatch(logout());
    if (query.status === "error") {
      logoutUser().catch((e: unknown) =>
        console.error("props.logout(history)", e)
      );
    }
  }, [query, history, dispatch]);

  switch (query.status) {
    case "loading":
    case "idle":
    default:
      return (
        <FormScreen
          title="Verifying your identity"
          buttons={<></>}
          navigation={{ type: "none" }}
        >
          <ActivityIndicator size="large" color="#FFF" />
        </FormScreen>
      );

    case "success":
      if (query.data === undefined) {
        throw new Error("data is undefined");
      }
      switch (query.data.type) {
        case "delay.KYC":
          return <Redirect to="/app/signup/kyc/delay" />;

        case "manual.kyc":
          return <Redirect to="/app/signup/kyc/manual" />;

        case "failed.kyc":
          return <Redirect to="/app/signup/kyc/failed" />;

        case "processing.kyc":
          return <Redirect to="/app/signup/kyc/processing" />;

        case "waiting.kyc":
          return (
            <KYCApproved
              gotoNext={() => {
                history.push(SettingsPaths.dashboard);
              }}
            />
          );

        case "mailed.kyc":
          return (
            <KYCApproved
              gotoNext={() => {
                history.push(SettingsPaths.dashboard);
              }}
            />
          );

        default:
          return <Redirect to="/app/signup/kyc/delay" />;
      }

    case "error":
      return (
        <FormScreen
          title="Verifying your identity"
          buttons={<></>}
          navigation={{ type: "none" }}
        >
          <ActivityIndicator size="large" color="#FFF" />
        </FormScreen>
      );
  }
};

export default KYCState;
