/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react/display-name */
import React, { useContext, useEffect, useRef } from "react";
import {
  ActivityIndicator,
  Animated,
  FlatList,
  SectionListData,
  StyleSheet,
  View,
} from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { useLayout } from "@react-native-community/hooks";

import SectionedList from "../../SectionedList/SectionedList";
import { Data, Section } from "../../SectionedList/Types";
import { ThemeContext } from "../../Theming/ThemeContext";

import { PurchaseProps } from "../Purchases/Types";
import PurchaseCard from "./PurchaseCard";
import { height } from "../../Banner/Banner";
import { ReferralProgress } from "../../../Actions/Referral/Types";

import TextPill from "./TextPill";
import Emoji from "./Emoji";
import PersonalLimit from "../Purchases/PersonalLimit";
import ReferallProgress from "./ReferralProgress";

export interface GotoProps {
  gotoPayment: () => void;
  gotoPurhcase: (id: string) => void;
}

interface Props extends GotoProps {
  waitlistStash: () => void;
  waitlistSave: () => void;
  noPurchasesYet: boolean;
  purchases: PurchaseProps[];
  stashGoals: {
    sentence: string;
    name: string;
    emoji: string;
    progress: number;
  }[];
  fsBanners: Data[];
  partnerBanners: Data[];
  canSpend: number;
  maxSpend: number;
  refProgress: null | ReferralProgress;
}

const Dashboard = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const insets = useSafeAreaInsets();
  const { onLayout, width } = useLayout();
  const fadeAnim = useRef(new Animated.Value(0)).current; // Initial value for opacity: 0

  useEffect(() => {
    Animated.timing(fadeAnim, {
      toValue: 1,
      duration: 500,
      delay: 100,
      useNativeDriver: true,
    }).start();
  }, [fadeAnim]);

  let emptyComp = <ActivityIndicator size="large" color={theme.textColor} />;
  if (width === 0) {
    emptyComp = <></>;
  }

  const purchases = (
    <View style={styles.purchaseContainer}>
      <FlatList
        horizontal
        data={props.purchases}
        ListHeaderComponent={() => <View style={styles.purchasePadder} />}
        ListFooterComponent={() => <View style={styles.purchasePadder} />}
        ItemSeparatorComponent={() => <View style={styles.purchasePadder} />}
        ListEmptyComponent={
          <Animated.View
            style={[
              styles.emptyPurchases,
              { width: width - 20, height: height },
              { opacity: fadeAnim },
            ]}
          >
            {emptyComp}
          </Animated.View>
        }
        renderItem={(p) => (
          <PurchaseCard {...p} gotoPurhcase={props.gotoPurhcase} />
        )}
      />
    </View>
  );

  let payTgthr: Data[] = [
    {
      type: "node",
      key: "Transactions",
      node: purchases,
    },
    {
      type: "node",
      key: "personal limit",
      node: (
        <View style={[{ paddingHorizontal: 10, marginBottom: 20 }]}>
          <PersonalLimit canSpend={props.canSpend} maxSpend={props.maxSpend} />
        </View>
      ),
    },
  ];

  if (props.refProgress !== null) {
    payTgthr = [
      ...payTgthr,
      {
        type: "node",
        key: props.refProgress.created,
        node: (
          <View style={[styles.plIncrease]}>
            <ReferallProgress progress={props.refProgress} />
          </View>
        ),
      },
    ];
  }

  const warningNodes: Data[] = [...props.partnerBanners, ...props.fsBanners];

  const data: SectionListData<Data, Section>[] = [
    {
      sectionType: "none",
      key: "warnings-section",
      data: warningNodes,
    },
    {
      sectionType: "text",
      title: "Pay Tgthr",
      data: payTgthr,
    },
    {
      sectionType: "text",
      title: "Stash Tgthr",
      data: [
        {
          type: "text",
          text: "Get on the waitlist",
          icon: (p) => <TextPill {...p} text="Coming soon" />,
          helpText: `Save for the bigger things in life with Stash Tgthr. Save for ${props.stashGoals[0].sentence}, ${props.stashGoals[1].sentence} or ${props.stashGoals[2].sentence} together!\n\nTap here to get on the waitlist`,
          action: props.waitlistStash,
        },
        {
          type: "text",
          text: props.stashGoals[0].name,
          icon: (p) => (
            <Emoji {...p} emoji={props.stashGoals[0].emoji} color="#3FA9D740" />
          ),
          progress: {
            value: props.stashGoals[0].progress,
            color: "#3FA9D7",
          },
        },
        {
          type: "text",
          text: props.stashGoals[1].name,
          icon: (p) => (
            <Emoji {...p} emoji={props.stashGoals[1].emoji} color="#F5D75940" />
          ),
          progress: {
            value: props.stashGoals[1].progress,
            color: "#F5D759",
          },
        },
      ],
    },
    {
      sectionType: "text",
      title: "Save Tgthr",
      data: [
        {
          type: "text",
          text: "Get on the waitlist",
          icon: (p) => <TextPill {...p} text="Coming later" />,
          helpText: `Build a rainy day fund together for life's unexpected moments.\n\nTap here to get on the waitlist`,
          action: props.waitlistSave,
        },
      ],
    },
  ];

  return (
    <View style={styles.container}>
      <theme.background style={[StyleSheet.absoluteFill]} />
      <theme.background style={[styles.topInset, { height: insets.top }]} />
      <SectionedList
        itemStyle={[styles.itemStyle]}
        sectionStyle={[styles.sectionlist]}
        onLayout={onLayout}
        ListFooterComponent={<View style={styles.screenFooter} />}
        data={data}
      />
    </View>
  );
};

export default Dashboard;

const styles = StyleSheet.create({
  container: { flex: 1 },
  emptyPurchases: {
    alignItems: "center",
    justifyContent: "center",
  },
  itemStyle: { marginBottom: 10, marginTop: 0 },
  plIncrease: { marginBottom: 10, paddingHorizontal: 10 },
  purchaseContainer: { height: 110, width: "100%" },
  purchasePadder: { width: 10 },
  screenFooter: { height: 20 },
  sectionlist: { marginHorizontal: 0 },
  topInset: { width: "100%" },
});
