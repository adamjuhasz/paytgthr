import React from "react";
import { ActivityIndicator, Share, StyleSheet, View } from "react-native";
import { useDispatch } from "react-redux";
import * as Clipboard from "expo-clipboard";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import { AlfieDispatch } from "../../State/Store";
import useGetInvite from "../Hooks/UseGetInvite";
import ViewInviteCode from "./ViewInviteCode";

interface Props {
  goBack: () => void;
}

const ViewInviteCodeHOC = (props: Props): JSX.Element => {
  const code = useGetInvite();
  const dispatch = useDispatch<AlfieDispatch>();

  if (code === null) {
    return (
      <View style={[styles.container]}>
        <ActivityIndicator />
      </View>
    );
  }

  const shareCode = async (code: string) => {
    const message = `Pay Tgthr invite code: ${code}\nhttps://paytgthr.com/getapp`;
    await Analytics.track("User InviteCode Shared", {
      code,
      message,
    });

    dispatch({
      type: "setTimeslot",
      key: "sharedPartnerInvite",
      value: new Date(),
    });

    const res = await Share.share({
      message: message,
    });

    switch (res.action) {
      case Share.dismissedAction:
        void Analytics.track("User InviteCode Dismissed", {
          action: res.action,
          code,
        });
        break;

      case Share.sharedAction:
        void Analytics.track("User InviteCode Shared", {
          action: res.action,
          activityType: res.activityType,
          code,
        });
        break;
    }
  };

  const copyCode = (code: string) => {
    void Analytics.track("User InviteCode Copied", { code });
    Clipboard.setString(`${code}`);
  };

  const formattedCode =
    code.length === 6 ? `${code.slice(0, 3)}-${code.slice(3)}` : code;

  return (
    <ViewInviteCode
      code={formattedCode}
      shareInvite={shareCode}
      copyInvite={copyCode}
      goBack={props.goBack}
    />
  );
};

export default ViewInviteCodeHOC;

const styles = StyleSheet.create({
  container: {
    alignItems: "center",
    height: "100%",
    justifyContent: "center",
    width: "100%",
  },
});
