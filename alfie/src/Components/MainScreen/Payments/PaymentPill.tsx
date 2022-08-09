/* eslint-disable react-native/no-color-literals */
import React from "react";
import { StyleSheet, Text, View } from "react-native";

import { Payment } from "../../../Actions/Payments/Type";
import { IconProps } from "../../Icons/Types";
import TextStyles from "../../Styling/Text";

export interface UpcomingPaymentProps {
  amount: number;
  date: Date;
}

export interface Credit {
  amount: number;
}

interface PaymentProps {
  payment: Payment;
}

const PaymentPill = ({ payment }: PaymentProps & IconProps): JSX.Element => {
  let pillColor = iconStyles.pillGreen;
  let pillText = (
    <Text style={[TextStyles.fontWeight500, iconStyles.pillGreen]}>
      Completed
    </Text>
  );

  const direction = payment.type === "CreditToUser" ? "Credit" : "Debit";

  switch (payment.status.tag) {
    case "PaymentCompleted":
      pillColor = iconStyles.pillGreen;
      pillText = (
        <Text style={[TextStyles.fontWeight500, iconStyles.pillGreen]}>
          {direction} completed
        </Text>
      );
      break;

    case "PaymentPending":
      pillColor =
        payment.type === "CreditToUser"
          ? iconStyles.pillYellow
          : iconStyles.pillGray;
      pillText = (
        <Text style={[TextStyles.fontWeight500, pillColor]}>
          {direction} started
        </Text>
      );
      break;

    case "PaymentFailed":
      pillColor = iconStyles.pillPink;
      pillText = (
        <Text style={[TextStyles.fontWeight500, iconStyles.pillPink]}>
          Failed {payment.status.contents.tag.slice(-3)}
        </Text>
      );
      break;
  }

  return (
    <View style={iconStyles.pillContainer}>
      <View style={[iconStyles.pill, pillColor]}>{pillText}</View>
    </View>
  );
};

export default PaymentPill;

export const iconStyles = StyleSheet.create({
  pill: {
    borderRadius: 40,
    paddingHorizontal: 10,
    paddingVertical: 5,
  },
  pillContainer: { alignItems: "center", flexDirection: "row" },
  pillGray: {
    backgroundColor: "#E3EFFD",
    color: "#244599",
  },
  pillGreen: {
    backgroundColor: "#E3F6ED",
    color: "#225240",
  },
  pillPink: {
    backgroundColor: "#FAE9E8",
    color: "#8F2923",
  },
  pillYellow: {
    backgroundColor: "#FEF3C7",
    color: "#D97706",
  },
});
