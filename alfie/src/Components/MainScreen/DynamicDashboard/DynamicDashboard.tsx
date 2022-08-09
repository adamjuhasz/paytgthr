import React, { useContext } from "react";
import { Redirect } from "../../../PlatformSpecific/react-router";
import { ActivityIndicator, StyleSheet, View } from "react-native";

import DashboardSetup, {
  Props as SetupProps,
} from "../SetupDashboard/DashboardHOC";
import Dashboard from "../Dashboard/DashboardHOC";
import { GotoProps } from "../Dashboard/Dashboard";
import useGetTransaction from "../../Hooks/UseGetTransactions";
import useGetUser from "../../Hooks/UseGetUser";
import SignUpPaths from "../../Routers/SignUpRouter/Paths";
import { userClosedPath } from "../../Routers/RootRouter/Paths";
import { ThemeContext } from "../../Theming/ThemeContext";

export default function DynamicDashboard(
  props: SetupProps & GotoProps
): JSX.Element {
  const theme = useContext(ThemeContext);
  const trx = useGetTransaction();
  const user = useGetUser();

  if (trx.status === "loading") {
    return (
      <>
        <View style={[StyleSheet.absoluteFill, styles.activity]}>
          <ActivityIndicator size="large" color={theme.textColor} />
        </View>
      </>
    );
  }

  if (user !== null && user.user.status.tag === "UserClosed") {
    return <Redirect to={userClosedPath} />;
  }

  if (
    user !== null &&
    (user.user.status.tag === "UserKYCDelay" ||
      user.user.status.tag === "UserWaitingOnKYC" ||
      user.user.status.tag === "UserUpdatedKYCDelay")
  ) {
    return <Redirect to={SignUpPaths.kycManual} />;
  }

  if (
    trx.data !== undefined &&
    trx.data
      .filter((t) => t.isDeclined === false)
      .filter((t) => t.amount !== "0.00").length > 0
  ) {
    return <Dashboard {...props} />;
  }

  return <DashboardSetup {...props} />;
}

const styles = StyleSheet.create({
  activity: { alignItems: "center", justifyContent: "center" },
});
