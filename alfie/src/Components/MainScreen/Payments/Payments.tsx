/* eslint-disable react-native/no-inline-styles */

import React, { useContext } from "react";
import { SectionListData, StyleSheet, Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { Payment } from "../../../Actions/Payments/Type";
import SectionedList from "../../SectionedList/SectionedList";
import { Data, Section, TextData } from "../../SectionedList/Types";
import { helpTextFontSize } from "../../SectionedList/TextItem";
import { ThemeContext } from "../../Theming/ThemeContext";
import TextStyles from "../../Styling/Text";

import UpcomingPayment from "./UpcomingPayment";
import { convertData } from "./ConvertData";
import {
  ghostPayments,
  ghostPendingPurchases,
  upcomingGhostPayments,
} from "./GhostPayments";

export interface UpcomingPaymentProps {
  amount: number;
  date: Date;
}

export interface Credit {
  amount: number;
}

interface Props {
  payments: Payment[];
  pendingPurchases: number;
  upcomingPayments: UpcomingPaymentProps[];
  credits: Credit[];
  demoMode: boolean;
}

const Payments = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const insets = useSafeAreaInsets();

  const finalPayments = !props.demoMode ? props.payments : ghostPayments;
  const upcomingPayments = !props.demoMode
    ? props.upcomingPayments
    : upcomingGhostPayments;
  const pendingPurchases = !props.demoMode
    ? props.pendingPurchases
    : ghostPendingPurchases;

  const convertedData: TextData[] = convertData(finalPayments);

  let upcoming: SectionListData<Data, Section>[] = [];
  if (upcomingPayments.length > 0) {
    upcoming = [
      {
        sectionType: "text",
        title: "Upcoming payments",
        data: upcomingPayments
          .filter((p) => Math.round(p.amount * 100) !== 0)
          .map((payment) => ({
            type: "text",
            text: `$${Math.abs(payment.amount).toFixed(2)}`,
            helpText: `Scheduled: ${
              payment.date.getMonth() + 1
            }/${payment.date.getDate()}/${payment.date.getFullYear()}`,
            // eslint-disable-next-line react/display-name
            icon: (props) => <UpcomingPayment {...props} {...payment} />,
          })),
      },
    ];
  }

  let credits: SectionListData<Data, Section>[] = [];
  if (props.credits.length > 0) {
    credits = [
      {
        sectionType: "text",
        title: "Credits",
        data: props.credits.map((credit) => ({
          type: "text",
          text: `$${Math.abs(credit.amount).toFixed(2)}`,
          helpText: "Buy something on us!",
        })),
      },
    ];
  }

  const sectionData: SectionListData<Data, Section>[] = [
    {
      sectionType: "text",
      title: "Pending purchases",
      data: [
        {
          type: "text",
          text: `$${pendingPurchases.toFixed(2)}`,
          helpText:
            "These are purchases your group has recently made. Once the merchant has finalized them, we'll schedule a payment for the next business day.",
        },
      ],
    },
    ...credits,
    ...upcoming,
    {
      sectionType: "text",
      title: "Recent payments",
      data: convertedData,
    },
  ];

  return (
    <View style={[styles.container]}>
      <theme.background style={[StyleSheet.absoluteFill]} />
      <theme.background style={[styles.topInset, { height: insets.top }]} />
      {props.demoMode ? (
        <View style={[styles.demo]}>
          <Text
            style={[
              TextStyles.fontWeight600,
              theme.textStyle,
              styles.demoTitle,
            ]}
          >
            Demo mode
          </Text>
          <Text
            style={[
              TextStyles.fontWeight400,
              theme.textStyle,
              styles.demoExplainer,
            ]}
          >
            Once you make payments, you'll see them below
          </Text>
        </View>
      ) : (
        <></>
      )}
      <SectionedList
        ListFooterComponent={<View style={styles.footerSpacer} />}
        data={sectionData}
        textStyle={styles.textStyle}
        helpTextStyle={styles.helpTextStyle}
        itemStyle={styles.itemStyle}
        style={[{ backgroundColor: theme.backgroundColor }]}
      />
    </View>
  );
};

export default Payments;

const styles = StyleSheet.create({
  container: { flex: 1 },
  demo: { marginBottom: 0, marginTop: 20, paddingHorizontal: 28 },
  demoExplainer: { fontSize: 15, textAlign: "center" },
  demoTitle: { fontSize: 28, textAlign: "center" },
  footerSpacer: { height: 20 },
  helpTextStyle: {
    fontSize: helpTextFontSize - 2,
    marginBottom: 0,
    marginTop: 0,
  },
  itemStyle: { marginBottom: 0, paddingHorizontal: 10 },
  textStyle: { marginBottom: 0, marginTop: 0 },
  topInset: { width: "100%" },
});
