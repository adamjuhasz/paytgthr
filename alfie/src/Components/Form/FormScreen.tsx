/* eslint-disable react-native/no-color-literals */
import React, { PropsWithChildren, useContext } from "react";
import {
  Keyboard,
  KeyboardAvoidingView,
  Platform,
  ScrollView,
  ScrollViewProps,
  StyleSheet,
  TouchableWithoutFeedback,
  View,
  ViewProps,
  ViewStyle,
  useColorScheme,
} from "react-native";
import { useDimensions, useKeyboard } from "@react-native-community/hooks";
import SafeAreaView from "../../PlatformSpecific/SafeAreaView";

import { ThemeContext } from "../Theming/ThemeContext";
import { FormScreenContext } from "./FormScreen/FormSceenContext";
import PageHeader, { LeftChevron } from "../PageHeader/PageHeader";
import { useScreenSize } from "../Styling/PhoneSizes";

export type Navigation =
  | { type: "action"; action: () => void }
  | { type: "none" };

interface Props {
  title?: string;
  buttons: React.ReactNode;
  navigation: Navigation;
  rightIcon?: React.ReactNode;
  rightAction?: () => void;
  testID?: string;
  disableInputFeatures?: boolean;
  style?: ViewStyle;
  innerStyle?: ViewStyle;
}

const KeyboardAvoiding = (props: PropsWithChildren<ViewProps>) => {
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const { children: _unused, ...nonChildrenProps } = props;
  switch (Platform.OS) {
    case "android":
      return <>{props.children}</>;

    case "ios":
      return (
        <KeyboardAvoidingView
          {...nonChildrenProps}
          behavior="padding"
          keyboardVerticalOffset={40}
        >
          {props.children}
        </KeyboardAvoidingView>
      );

    default:
      return <>{props.children}</>;
  }
};

const KeyboardDismisser = (props: PropsWithChildren<unknown>) => {
  const { keyboardShown } = useKeyboard();

  switch (Platform.OS) {
    case "web":
      return <>{props.children}</>;

    default:
      return (
        <TouchableWithoutFeedback
          onPress={() => {
            Keyboard.dismiss();
          }}
          disabled={!keyboardShown}
        >
          {props.children}
        </TouchableWithoutFeedback>
      );
  }
};

const FormScreen = (props: PropsWithChildren<Props>): JSX.Element => {
  const theme = useContext(ThemeContext);
  const dims = useDimensions();
  const { deviceClass } = useScreenSize();
  const prevPathCB = () => {
    switch (props.navigation.type) {
      case "action":
        props.navigation.action();
        return;

      case "none":
        return;
    }
  };

  let colorScheme = useColorScheme();
  if (colorScheme === null) {
    colorScheme = "light";
  }

  let leftIcon: JSX.Element | undefined;
  let rightIcon = props.rightIcon;
  switch (props.navigation.type) {
    case "action":
      leftIcon = <LeftChevron color={theme.textColor} size={20} />;
      break;

    case "none":
      break;
  }
  switch (deviceClass) {
    case "tablet":
    case "desktop":
      leftIcon = <></>;
      rightIcon = <></>;
      break;

    case "miniPhone":
    case "phone":
      break;
  }

  const header =
    props.title === undefined ? (
      <></>
    ) : (
      <PageHeader
        title={props.title}
        leftAction={prevPathCB}
        leftIcon={leftIcon}
        rightAction={props.rightAction}
        rightIcon={rightIcon}
      />
    );

  const enableSpecial =
    props.disableInputFeatures === undefined
      ? true
      : !props.disableInputFeatures;

  return (
    <FormScreenContext.Provider value={dims.window.width * 0.8}>
      <View style={[StyleSheet.absoluteFill, styles.scaled, props.style]}>
        <theme.background style={StyleSheet.absoluteFill} />
        <SafeAreaView style={styles.fullScreenPercent}>
          <View style={[styles.fullScreenPercent]} testID={props.testID}>
            <View style={StyleSheet.absoluteFill}>
              <theme.background />
            </View>
            <FormSpecialFeatures
              enableSpecial={enableSpecial}
              header={header}
              buttons={props.buttons}
              innerStyle={props.innerStyle}
            >
              {props.children}
            </FormSpecialFeatures>
          </View>
        </SafeAreaView>
      </View>
    </FormScreenContext.Provider>
  );
};

interface FormSpecialFeaturesProps {
  enableSpecial: boolean;
  header: JSX.Element;
  buttons: React.ReactNode;
  innerStyle?: ViewStyle;
}

const FormSpecialFeatures = (
  props: React.PropsWithChildren<FormSpecialFeaturesProps>
) => {
  const scrollViewOptions: ScrollViewProps = {
    style: styles.fullScreenPercent,
    contentContainerStyle: styles.mainContents,
    keyboardShouldPersistTaps: "handled",
    alwaysBounceVertical: false,
    indicatorStyle: "white",
  };

  if (props.enableSpecial) {
    return (
      <KeyboardAvoiding>
        <ScrollView {...scrollViewOptions}>
          {props.header}
          <KeyboardDismisser>
            <View style={styles.content}>{props.children}</View>
          </KeyboardDismisser>
          <View style={styles.empty}>{props.buttons}</View>
        </ScrollView>
      </KeyboardAvoiding>
    );
  } else {
    return (
      <View style={[styles.fullScreenPercent, styles.specialContainer]}>
        {props.header}
        <View
          style={[
            styles.content,
            styles.specialChildrenContainer,
            props.innerStyle,
          ]}
        >
          {props.children}
        </View>
        <View style={styles.empty}>{props.buttons}</View>
      </View>
    );
  }
};

export default FormScreen;

const styles = StyleSheet.create({
  content: {
    flexDirection: "column",
    flexGrow: 2,
    justifyContent: "center",
    padding: 10,
    paddingTop: 0,
  },
  empty: {
    alignItems: "center",
    flexDirection: "column",
    padding: 10,
    paddingBottom: 0,
    paddingTop: 0,
  },
  fullScreenPercent: {
    height: "100%",
    width: "100%",
  },
  mainContents: {
    flexDirection: "column",
    flexGrow: 1,
    justifyContent: "space-between",
  },
  scaled: {
    overflow: "hidden",
  },
  specialChildrenContainer: { flex: 1, overflow: "hidden" },
  specialContainer: { flexDirection: "column" },
});
