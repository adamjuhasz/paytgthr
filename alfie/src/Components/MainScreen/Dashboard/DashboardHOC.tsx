import React, { useMemo } from "react";
import Alert from "../../../PlatformSpecific/Alert";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";
import { defaultTo, random, sampleSize, take } from "lodash";

import useGetTransaction from "../../Hooks/UseGetTransactions";
import useGetLevel from "../../Hooks/UseGetLevel";
import { useGetMyReferralProgress } from "../../../Actions/Referral/GetReferralProgress";

import Dashboard, { GotoProps } from "./Dashboard";
import { useMakePartnershipBanner } from "./InfoBoxes/Partnership";
import { useMakeFSBanner } from "./InfoBoxes/FundingSource";
import { wrapNodes } from "./InfoBoxes/Utils";
import { goalList } from "./DemoGoals";

const SettingsHOC = (props: GotoProps): JSX.Element => {
  const goals = useMemo(
    () =>
      sampleSize(goalList, 3).map((g) => ({ ...g, progress: random(30, 90) })),
    []
  );

  const transactionQuery = useGetTransaction();

  const fsBanners = useMakeFSBanner();
  const partnerBanners = useMakePartnershipBanner();
  const levels = useGetLevel();
  const refProgressQuery = useGetMyReferralProgress();

  const refProgress = defaultTo(refProgressQuery.data, null);
  const transactions = take(defaultTo(transactionQuery.data, []), 5);
  const noPurchasesYet =
    transactionQuery.status === "success" && transactions.length === 0;

  const waitlistSave = () => {
    void Analytics.track("User WaitlistRequest SaveTgthr");
    Alert.alert(
      "Join waitlist",
      "Do you want to join the waitlist for Save Tgthr?",
      [
        {
          text: "Yes",
          style: "default",
          onPress: () => {
            void Analytics.track("User WaitlistRequestConfirm SaveTgthr");
            Alert.alert("You're on the list!");
          },
        },
        {
          text: "Nope",
          style: "cancel",
          onPress: () => {
            void Analytics.track("User WaitlistRequestCancel SaveTgthr");
          },
        },
      ]
    );
  };

  const waitlistStash = () => {
    void Analytics.track("User WaitlistRequest StashTgthr");
    Alert.alert(
      "Join waitlist",
      "Do you want to join the waitlist for Stash Tgthr?",
      [
        {
          text: "Yes",
          style: "default",
          onPress: () => {
            void Analytics.track("User WaitlistRequestConfirm StashTgthr");
            Alert.alert("You're on the list!");
          },
        },
        {
          text: "Nope",
          style: "cancel",
          onPress: () => {
            void Analytics.track("User WaitlistRequestCancel StashTgthr");
          },
        },
      ]
    );
  };

  return (
    <Dashboard
      waitlistSave={waitlistSave}
      waitlistStash={waitlistStash}
      noPurchasesYet={noPurchasesYet}
      purchases={transactions}
      stashGoals={goals}
      fsBanners={wrapNodes(fsBanners)}
      partnerBanners={wrapNodes(partnerBanners)}
      canSpend={levels.data.canSpend}
      maxSpend={levels.data.maxSpend}
      refProgress={refProgress}
      {...props}
    />
  );
};

export default SettingsHOC;
