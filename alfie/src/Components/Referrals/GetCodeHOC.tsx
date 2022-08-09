import React from "react";
import { ActivityIndicator, Share, StyleSheet, View } from "react-native";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import * as Clipboard from "expo-clipboard";

import { useGetReferralCode } from "../../Actions/Referral/GetRefCode";
import GetCode from "./GetCode";

export const path = "/app/referral/code";

interface Props {
  goBack: () => void;
}

export default function GetReferralCode(props: Props): JSX.Element {
  const codeQuery = useGetReferralCode();

  if (codeQuery.data === undefined) {
    return (
      <View style={[styles.container]}>
        <ActivityIndicator />
      </View>
    );
  }

  const shareCode = async (code: string) => {
    const message = `Pay Tgthr referral code: ${code}\nhttps://paytgthr.com/getapp`;

    const res = await Share.share({
      message: message,
    });

    switch (res.action) {
      case Share.dismissedAction:
        void Analytics.track("User ReferralCode Dismissed", {
          action: res.action,
          code,
        });
        break;

      case Share.sharedAction:
        void Analytics.track("User ReferralCode Shared", {
          action: res.action,
          activityType: res.activityType,
          code,
        });
        break;
    }
  };

  const copyCode = (code: string) => {
    void Analytics.track("User ReferralCode Copied", { code });
    Clipboard.setString(`${code}`);
  };

  return (
    <GetCode
      code={codeQuery.data.code}
      goBack={props.goBack}
      shareCode={shareCode}
      copyCode={copyCode}
    />
  );
}

const styles = StyleSheet.create({
  container: {
    alignItems: "center",
    height: "100%",
    justifyContent: "center",
    width: "100%",
  },
});
