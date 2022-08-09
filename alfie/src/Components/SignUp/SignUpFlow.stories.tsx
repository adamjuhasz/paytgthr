/* eslint-disable @typescript-eslint/require-await */
/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { StyleSheet, View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import SignUpComp from "../SignupForm/SignUpForm";
import PhoneEntryComp from "./PhoneEntry/PhoneEntry";
import PhoneVerifyComp from "./PhoneVerify/PhoneVerify";
import NameComp from "./Name/Name";
import AddressComp from "./Address/Address";
import LegalConsentComp from "./LegalConsent/LegalConsent";
import SSNEntryComp from "./SSN/SSNEntry";
import DOBEntryComp from "./DateOfBirth/DOBEntry";
import Survey from "./Survey/Survey";

export default {
  title: "SignUpFlow",
} as Meta;

export const CreateAccount: Story<
  React.ComponentProps<typeof SignUpComp>
> = () => (
  <SignUpComp
    inProgress={false}
    signup={() => 1}
    emailError="None"
    passwordError="None"
    goBack={() => 1}
    screenIndex={0}
    screenCount={7}
  />
);

export const EnterPhone: Story<React.ComponentProps<typeof PhoneEntryComp>> = (
  args
) => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <PhoneEntryComp {...args} />
    </SafeAreaProvider>
  </View>
);
EnterPhone.args = {
  error: null,
  screenIndex: 1,
  screenCount: 7,
  inProgress: false,
  phone: "",
};

export const VerifyPhone: Story<
  React.ComponentProps<typeof PhoneVerifyComp>
> = (args) => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <PhoneVerifyComp {...args} />
    </SafeAreaProvider>
  </View>
);
EnterPhone.args = {
  error: null,
  screenIndex: 1,
  screenCount: 7,
  inProgress: false,
  phone: "9492853343",
};

export const EnterName: Story<React.ComponentProps<typeof NameComp>> = (
  args
) => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <NameComp {...args} />
    </SafeAreaProvider>
  </View>
);
EnterName.args = {
  screenIndex: 2,
  screenCount: 7,
  inProgress: false,
};

export const AcceptLegal: Story<
  React.ComponentProps<typeof LegalConsentComp>
> = (args) => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <LegalConsentComp {...args} />
    </SafeAreaProvider>
  </View>
);
AcceptLegal.args = {
  inProgress: false,
  screenCount: 7,
  screenIndex: 3,
};

export const EnterAddress: Story<React.ComponentProps<typeof AddressComp>> = (
  args
) => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <AddressComp {...args} />
    </SafeAreaProvider>
  </View>
);
EnterAddress.args = {
  inProgress: false,
  errors: { street: "None", city: "None", state: "None", zip: "None" },
  screenIndex: 4,
  screenCount: 7,
};

export const EnterDOB: Story<React.ComponentProps<typeof DOBEntryComp>> = (
  args
) => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <DOBEntryComp {...args} />
    </SafeAreaProvider>
  </View>
);
EnterDOB.args = {
  usersName: "Tom Jone's",
  inProgress: false,
  dob: "",
  error: null,
  screenIndex: 5,
  screenCount: 7,
};

export const EnterSSN: Story<React.ComponentProps<typeof SSNEntryComp>> = (
  args
) => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <SSNEntryComp {...args} />
    </SafeAreaProvider>
  </View>
);
EnterSSN.args = {
  inProgress: false,
  ssn: "",
  error: null,
  screenIndex: 6,
  screenCount: 7,
};

export const SurveyScreen: Story<React.ComponentProps<typeof Survey>> = () => (
  <View style={StyleSheet.absoluteFill}>
    <SafeAreaProvider>
      <Survey gotoNext={() => 1} gotoSurvey={() => 1} />
    </SafeAreaProvider>
  </View>
);
