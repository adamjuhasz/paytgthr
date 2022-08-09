/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */
import React, { useContext, useState } from "react";
import {
  ActivityIndicator,
  Platform,
  Pressable,
  ScrollView,
  StatusBar,
  StyleSheet,
  Text,
  View,
  ViewStyle,
} from "react-native";
import { LinearGradient } from "expo-linear-gradient";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { defaultTo } from "lodash";

import { Reward } from "../../Hooks/UseGetOurRewards";
import TextStyles from "../../Styling/Text";
import PlusCircle from "./PlusCircle";
import Checkmark from "./Checkmark";
import { BoostDB } from "./BoostDB";
import LeftChevron from "../../Icons/LeftChevron";
import { ThemeContext } from "../../Theming/ThemeContext";

interface Props {
  rewards: Reward[];
  selected: Reward[];
  level: string;
  activateBoost: (boostId: string) => void;
  goBack: () => void;
  inProgress: boolean;
}

export default function BoostActivator(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);
  const [backPressed, setBackPressed] = useState(false);
  const { top } = useSafeAreaInsets();
  const selected = props.selected.map((r) => r.boostId);

  const LevelIndicator = (levelProps: { style?: ViewStyle }) => (
    <View
      style={[
        {
          width: 60,
          height: 60,
          backgroundColor: "#FFFFFF",
          borderRadius: 999,
          position: "absolute",
          bottom: -30,
          justifyContent: "center",
          alignItems: "center",
          zIndex: 100,
        },
        levelProps.style,
      ]}
    >
      <Text
        style={[
          TextStyles.fontWeight600,
          { fontSize: 30, marginLeft: 8, marginTop: -3 },
        ]}
      >
        {parseInt(props.level) / 100}
        <Text style={[{ fontSize: 14 }]}>%</Text>
      </Text>
    </View>
  );

  let iosHeader = (
    <View style={[{ width: "100%", height: 1, alignItems: "center" }]}>
      <LevelIndicator />
    </View>
  );
  let androidHeader = (
    <View
      style={[
        {
          width: "100%",
          height: 0,
          alignItems: "center",
          position: "absolute",
          top: top + 65,
        },
      ]}
    >
      <LevelIndicator
        style={{
          shadowOffset: {
            width: 0,
            height: 9,
          },
          shadowOpacity: 0.48,
          shadowRadius: 11.95,

          elevation: 18,
          position: "absolute",
          top: 0,
        }}
      />
    </View>
  );
  if (Platform.OS === "ios") {
    androidHeader = <></>;
  } else {
    iosHeader = <></>;
  }

  let headerText = "";
  let mainText = "";

  switch (props.level) {
    case "500":
      headerText = "Date night";
      mainText =
        "Save on everything you need have a night together, from movies to restaurants to streaming at home";
      break;

    case "300":
      headerText = "Home life";
      mainText =
        "Save on everyday home expenses like utilities, groceries, insuarance (remember to adult), and something for Rover too";
      break;
  }

  return (
    <View
      style={[
        StyleSheet.absoluteFill,
        {
          flexDirection: "column",
        },
      ]}
    >
      <View
        style={[
          {
            borderBottomLeftRadius: 20,
            borderBottomRightRadius: 20,
          },
          {
            shadowColor: "#2A82D4",
            shadowOffset: {
              width: 0,
              height: 9,
            },
            shadowOpacity: 0.48,
            shadowRadius: 11.95,

            elevation: 18,
          },
        ]}
      >
        <View
          style={[
            {
              paddingTop: top + (Platform.OS === "ios" ? 17 : 24),
              height: top + (Platform.OS === "ios" ? 17 : 24) + 73,
            },
            {
              backgroundColor: "#2A82D4",
              flexDirection: "column",
              paddingBottom: 24,
              borderBottomLeftRadius: 20,
              borderBottomRightRadius: 20,
              overflow: "hidden",
            },
          ]}
        >
          {Platform.OS === "ios" ? (
            <StatusBar barStyle="light-content" animated />
          ) : (
            <></>
          )}
          <LinearGradient
            style={[
              StyleSheet.absoluteFill,
              {
                borderBottomLeftRadius: 20,
                borderBottomRightRadius: 20,
                overflow: "hidden",
              },
            ]}
            colors={["#49A1E0", "#2A82D4"]}
            start={{ x: 0, y: 0.5 }}
            end={{ x: 1.0, y: 0.5 }}
          />
          <Pressable
            onPress={() => props.goBack()}
            onPressIn={() => setBackPressed(true)}
            onPressOut={() => setBackPressed(false)}
            style={[
              { flexDirection: "row", alignItems: "center", paddingLeft: 14 },
              { opacity: backPressed ? 0.5 : 1.0 },
            ]}
          >
            <LeftChevron color="#FFFFFF" size={18} />
            <Text
              style={[
                TextStyles.fontWeight600,
                { fontSize: 16, color: "#FFFFFF" },
              ]}
            >
              Back
            </Text>
          </Pressable>
        </View>
        {iosHeader}
      </View>
      <ScrollView style={[{ marginTop: 38 }]}>
        <View
          style={[
            {
              marginTop: 30,
              width: "100%",
              alignItems: "center",
              paddingHorizontal: 28,
            },
          ]}
        >
          <Text
            style={[
              TextStyles.fontWeight600,
              theme.textStyle,
              { fontSize: 28 },
            ]}
          >
            {headerText}
          </Text>
          <Text
            style={[
              TextStyles.fontWeight400,
              theme.textStyle,
              { textAlign: "center", fontSize: 15, marginTop: 8 },
            ]}
          >
            {mainText}
          </Text>
        </View>
        <View style={[{ height: 40 - 12 }]} />
        {props.rewards.map((r) => (
          <View
            key={r.boostId}
            style={[
              {
                marginVertical: 12,
              },
            ]}
          >
            <View
              style={[
                {
                  flexDirection: "row",
                  justifyContent: "space-between",
                  paddingHorizontal: 28,
                  alignItems: "center",
                },
              ]}
            >
              <View
                style={[
                  {
                    flexDirection: "row",
                    alignItems: "center",
                    width: "100%",
                    flex: 1,
                  },
                ]}
              >
                <View
                  style={[
                    {
                      width: 40,
                      height: 40,
                      borderRadius: 999,
                      backgroundColor: "#F65E93",
                      justifyContent: "center",
                      alignItems: "center",
                      overflow: "hidden",
                    },
                  ]}
                >
                  <LinearGradient
                    style={[
                      StyleSheet.absoluteFill,
                      {
                        borderRadius: 10,
                        overflow: "hidden",
                      },
                    ]}
                    colors={["#F65E93", "#FD7776"]}
                    start={{ x: 0.5, y: 0.0 }}
                    end={{ x: 0.5, y: 1.0 }}
                  />
                  <Text style={[theme.textStyle, { fontSize: 20 }]}>
                    {BoostDB[r.boostId]?.emoji}
                  </Text>
                </View>
                <View style={[{ marginLeft: 12, flex: 1 }]}>
                  <Text
                    style={[
                      TextStyles.fontWeight600,
                      theme.textStyle,
                      { fontSize: 15 },
                    ]}
                  >
                    {defaultTo(BoostDB[r.boostId]?.friendlyName, r.boostName)}
                  </Text>
                  <Text
                    style={[
                      TextStyles.fontWeight400,
                      theme.textStyle,
                      { fontSize: 15, flex: 1, paddingRight: 8 },
                    ]}
                  >
                    {BoostDB[r.boostId]?.description}
                  </Text>
                </View>
              </View>
              <Pressable
                onPress={() => props.activateBoost(r.boostId)}
                disabled={props.inProgress}
              >
                {props.inProgress ? (
                  <ActivityIndicator
                    size="small"
                    style={{ width: 32, height: 32 }}
                    color="#F65E93"
                  />
                ) : selected.includes(r.boostId) ? (
                  <Checkmark />
                ) : (
                  <PlusCircle color="#F65E93" />
                )}
              </Pressable>
            </View>
          </View>
        ))}
        <View style={[{ height: 40 }]} />
      </ScrollView>
      {androidHeader}
    </View>
  );
}
