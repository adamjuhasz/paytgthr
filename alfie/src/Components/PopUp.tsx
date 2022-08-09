/* eslint-disable react-native/no-unused-styles */
import React, { useCallback, useContext, useRef, useState } from "react";
import {
  Animated,
  Keyboard,
  Platform,
  StyleSheet,
  TouchableOpacity,
  View,
} from "react-native";
import SafeAreaView from "../PlatformSpecific/SafeAreaView";
import { useLayout } from "@react-native-community/hooks";
import Analytics from "../PlatformSpecific/SegmentAnalytcs";
import { blackColor, transparentColor, whiteColor } from "./Styling/Colors";
import TextStyles from "./Styling/Text";
import InformationIcon from "./Icons/Information";
import { ThemeContext } from "./Theming/ThemeContext";

interface PopUpContext {
  popUp: (popUpName: string, content: React.ReactNode) => void;
}

export const PopUpContext = React.createContext<PopUpContext>({
  popUp: () => undefined,
});
PopUpContext.displayName = "PopUpContext";

const PopUpProvider = (
  props: React.PropsWithChildren<unknown>
): JSX.Element => {
  const [popUpNode, setNode] = useState<React.ReactNode>(
    <View style={styles.smallView} />
  );
  const [visible, setVisible] = useState(false);
  const { onLayout } = useLayout();
  const transformY = useRef(new Animated.Value(0));

  const popThis = useCallback(
    async (name: string, content: React.ReactNode) => {
      setNode(content);
      setVisible(true);
      Keyboard.dismiss();
      await Analytics.track("PopUp Opened", { name });
    },
    [setNode, setVisible]
  );

  const closePopup = useCallback(() => {
    setVisible(false);
    setNode(<View style={styles.smallView} />);
  }, [setVisible, setNode]);

  let cotentToShow = <></>;
  if (visible === true) {
    cotentToShow = (
      <TouchableOpacity style={styles.background} onPress={closePopup}>
        <SafeAreaView>
          <View style={styles.grower}>
            <Animated.View
              onLayout={onLayout}
              style={[
                styles.container,
                { transform: [{ translateY: transformY.current }] },
              ]}
            >
              {popUpNode}
            </Animated.View>
          </View>
        </SafeAreaView>
      </TouchableOpacity>
    );
  }

  return (
    <>
      <PopUpContext.Provider value={{ popUp: popThis }}>
        {props.children}
      </PopUpContext.Provider>
      {cotentToShow}
    </>
  );
};

export default PopUpProvider;

export const ShrugIcon = (): JSX.Element => {
  const theme = useContext(ThemeContext);
  return <InformationIcon size={20} color={theme.textColor} />;
};

const backgroundColor = "rgba(0,0,0, 0.5)";

const styles = StyleSheet.create({
  background: {
    backgroundColor: backgroundColor,
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
  container: {
    backgroundColor: whiteColor,
    borderRadius: 14,
    bottom: 0,
    left: 0,
    marginBottom: 7,
    marginHorizontal: 14,
    padding: 14,
    position: "absolute",
    right: 0,
  },
  grower: {
    height: "100%",
    width: "100%",
  },
  smallView: {
    backgroundColor: transparentColor,
    height: 1,
  },
});

const greyLineColor = "#E5E5E5";

export const PopUpStyles = StyleSheet.create({
  emojiIcon: {
    fontSize: Platform.OS === "ios" ? 30 : 24,
    marginRight: 10,
  },
  header: {
    color: blackColor,
    fontSize: 18,
    ...TextStyles.fontWeight600,
  },
  line: {
    backgroundColor: greyLineColor,
    height: 2,
    marginVertical: 14,
    width: "100%",
  },
  text: {
    color: blackColor,
    fontSize: 16,
    ...TextStyles.fontWeight400,
  },
});
