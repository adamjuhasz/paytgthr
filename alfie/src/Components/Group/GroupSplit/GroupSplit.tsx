/* eslint-disable react-native/no-color-literals */
import React, { useContext, useState } from "react";
import { FlatList, Platform, StyleSheet, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { LinearGradient } from "expo-linear-gradient";
import { defaultTo } from "lodash";

import { Category } from "../../../Actions/Groups/CategorySplits";
import FromScreen, { Navigation } from "../../Form/FormScreen";
import Button from "../../Button/Button";
import ProgressButton from "../../Button/ProgressButton";
import { buttonHeight } from "../../Button/Styling";
import PopUp from "../../SignUp/PopUp/PopUp";
import { ThemeContext } from "../../Theming/ThemeContext";
import CategoryRow from "./GroupSplitRow";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";

export const screenName = "GroupSplit";

const helpText = `Choose how you’d like to split your purchase with your partner, you can either: 

• Set a single split for all purchase by disabling all categories

• Choose how you’d like to split purchases by category one by one. 

Not sure if you’d like to split by category yet? No worries! You can come back to this page any time and change your splits to suit your needs.`;

interface Props {
  categoryList: Category[];
  showHelp?: boolean;
  navigation: Navigation;
  submit: (cats: Category[]) => void;
  userInitial?: string;
  partnerInitial?: string;
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
}

const GroupSplit = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const [cats, setCats] = useState(props.categoryList);
  const insets = useSafeAreaInsets();
  const [showHelp, setShow] = useState<boolean>(
    defaultTo(props.showHelp, false)
  );

  const modifyCat = (newCat: Category) => {
    const newCats = cats.map((c) => {
      if (c.id === newCat.id) {
        return newCat;
      } else {
        return c;
      }
    });
    setCats(newCats);
  };

  return (
    <>
      <FromScreen
        title="Purchase splitting"
        testID={`${screenName} FormScreen`}
        buttons={
          <>
            <Button
              style="Secondary"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "GroupSplit",
                });
              }}
              text="What is this?"
            />
            {props.navigation.type === "none" ? (
              <ProgressButton
                style="Primary"
                onPress={() => {
                  props.submit(cats);
                }}
                text="Next"
                disabled={false}
                index={props.screenIndex}
                count={props.screenCount}
              />
            ) : (
              <Button
                style="Primary"
                onPress={() => {
                  props.submit(cats);
                }}
                text="Continue"
                disabled={false}
              />
            )}
            <View style={styles.bottomButtonSpacer} />
          </>
        }
        navigation={props.navigation}
        disableInputFeatures={true}
      >
        <>
          <FlatList
            style={{
              marginBottom:
                insets.bottom + (Platform.OS === "android" ? 10 : -10),
            }}
            data={cats}
            renderItem={(c) => (
              <CategoryRow
                key={c.item.id}
                cat={c.item}
                modify={modifyCat}
                userInitial={defaultTo(props.userInitial, "Me")}
                partnerInitial={defaultTo(props.partnerInitial, "P")}
              />
            )}
            ListFooterComponent={<View style={styles.listFooter} />}
          />
        </>
      </FromScreen>
      <LinearGradient
        colors={[`${theme.backgroundColor}00`, `${theme.backgroundColor}FF`]}
        start={{ x: 0.5, y: 0.0 }}
        end={{ x: 0.5, y: 0.99 }}
        style={[
          styles.gradient,
          {
            bottom:
              insets.bottom +
              buttonHeight +
              10 +
              (Platform.OS === "android" ? 10 : -10),
          },
        ]}
      />
      <PopUp
        headingText="Set Your Splits!"
        bodyText={helpText}
        buttonText="Got it"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};

export default GroupSplit;

const styles = StyleSheet.create({
  bottomButtonSpacer: { height: Platform.OS === "android" ? 20 : 0 },
  gradient: {
    height: 30,
    position: "absolute",
    width: "100%",
  },
  listFooter: { height: Platform.OS === "android" ? 90 : 20 },
});
