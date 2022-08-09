/* eslint-disable react-native/no-color-literals */
import React from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen from "../../../Form/FormScreen";
import Button from "../../../Button/Button";
import SectionedList from "../../../SectionedList/SectionedList";
import { TextData } from "../../../SectionedList/Types";
import RightChevron from "../../../Icons/RightChevron";
import { IconProps } from "../../../Icons/Types";

export interface Account {
  bankName: string;
  accountName: string;
  verified: boolean;
}

interface Props {
  goBack: () => void;
  gotoVerify: () => void;
  gotoAddNew: () => void;
  accounts: Account[];
}

const Icon = (props: Account & IconProps) => (
  <View style={styles.pillContainer}>
    <View
      style={[styles.pill, props.verified ? styles.pillGreen : styles.pillPink]}
    >
      {props.verified ? (
        <Text style={[props.verified ? styles.pillGreen : styles.pillPink]}>
          Verified
        </Text>
      ) : (
        <Text style={[props.verified ? styles.pillGreen : styles.pillPink]}>
          Unverified
        </Text>
      )}
    </View>
    {props.verified ? undefined : (
      <View style={styles.chevron}>
        <RightChevron {...props} />
      </View>
    )}
  </View>
);

const FundingSourceList = (props: Props): JSX.Element => {
  const convertedData: TextData[] = props.accounts.map((account) => ({
    type: "text",
    text: account.accountName,
    helpText: `${account.bankName}`,
    // eslint-disable-next-line react/display-name
    icon: (props) => <Icon {...props} {...account} />,
    action: account.verified ? undefined : props.gotoVerify,
  }));
  return (
    <FormScreen
      title="Funding sources"
      navigation={{ type: "action", action: props.goBack }}
      disableInputFeatures={true}
      buttons={
        <>
          <Button
            text={
              props.accounts.length === 0
                ? "Add new account"
                : "Replace account"
            }
            onPress={props.gotoAddNew}
            style={props.accounts.length === 0 ? "Primary" : "Secondary"}
          />
        </>
      }
    >
      <SectionedList
        textStyle={styles.noMargin}
        helpTextStyle={styles.noMargin}
        data={[
          {
            sectionType: "text",
            title: "Linked accounts",
            data: convertedData,
          },
        ]}
      />
    </FormScreen>
  );
};

export default FundingSourceList;

const styles = StyleSheet.create({
  chevron: { marginLeft: 10 },
  noMargin: { marginBottom: 0, marginTop: 0 },
  pill: {
    borderRadius: 40,
    paddingHorizontal: 10,
    paddingVertical: 5,
  },
  pillContainer: { alignItems: "center", flexDirection: "row" },
  pillGreen: {
    backgroundColor: "#E3F6ED",
    color: "#225240",
  },
  pillPink: {
    backgroundColor: "#FF6347",
    color: "#FFFFFF",
  },
});
