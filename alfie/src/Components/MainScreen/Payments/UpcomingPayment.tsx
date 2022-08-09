/* eslint-disable react-native/no-color-literals */
import React from "react";
import { Text, View } from "react-native";

import { IconProps } from "../../Icons/Types";
import TextStyles from "../../Styling/Text";

import { iconStyles } from "./PaymentPill";

export interface UpcomingPaymentProps {
  amount: number;
  date: Date;
}

export interface Credit {
  amount: number;
}

const UpcomingPayment = ({
  amount,
}: UpcomingPaymentProps & IconProps): JSX.Element => {
  const pillColor = iconStyles.pillGray;
  let pillText = (
    <Text style={[TextStyles.fontWeight500, iconStyles.pillGreen]}>
      Completed
    </Text>
  );
  if (amount < 0) {
    pillText = (
      <Text style={[TextStyles.fontWeight500, iconStyles.pillGray]}>Debit</Text>
    );
  } else {
    pillText = (
      <Text style={[TextStyles.fontWeight500, iconStyles.pillYellow]}>
        Credit
      </Text>
    );
  }

  return (
    <View style={iconStyles.pillContainer}>
      <View style={[iconStyles.pill, pillColor]}>{pillText}</View>
    </View>
  );
};

export default UpcomingPayment;
