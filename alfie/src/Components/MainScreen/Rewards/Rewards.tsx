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
} from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { LinearGradient } from "expo-linear-gradient";
import { concat, defaultTo, keys, truncate } from "lodash";

import { Reward } from "../../Hooks/UseGetOurRewards";
import { Entry } from "../../Hooks/UseGetRewardEntries";
import TextStyles from "../../Styling/Text";
import { BoostDB } from "./BoostDB";

import FivePercent from "./FivePercent";
import ThreePercent from "./ThreePercent";
import OnePercent from "./OnePercent";
import PlusCircle from "./PlusCircle";
import DashedCircle from "./DashedCircle";
import { ThemeContext } from "../../Theming/ThemeContext";

interface Props {
  FiveRewards: Reward[];
  ThreeRewards: Reward[];
  selected: Record<string, Reward[] | undefined>;
  entries: Entry[];
  balance: number;
  gotoActivator: (level: string) => void;
  transfer: (amount: number) => void;
  transferInProgrss: boolean;
}

const BoostIcon = ({ boost }: { boost: Reward | undefined }) =>
  boost === undefined ? (
    <PlusCircle width={44} height={44} />
  ) : (
    <View
      style={[
        {
          width: 44,
          height: 44,
          borderRadius: 999,
          borderWidth: 1,
          borderColor: "#FFFFFF",
          justifyContent: "center",
          alignItems: "center",
        },
      ]}
    >
      <Text style={{ fontSize: 21 }}>
        {defaultTo(BoostDB[boost.boostId]?.emoji, "💳")}
      </Text>
    </View>
  );

export default function Rewards(props: Props): JSX.Element {
  const { top } = useSafeAreaInsets();
  const [fivePressed, setFivePressed] = useState(false);
  const [threePressed, setThreePressed] = useState(false);
  const [transferPressed, setTransferPressed] = useState(false);
  const theme = useContext(ThemeContext);

  const db: Record<string, Reward> = {};
  props.FiveRewards.forEach((r) => {
    db[r.boostId] = r;
  });
  props.ThreeRewards.forEach((r) => {
    db[r.boostId] = r;
  });

  const selected500 = defaultTo(props.selected[500], []);
  const selected300 = defaultTo(props.selected[300], []);

  const otherBips = keys(props.selected).filter(
    (b) => b !== "500" && b !== "300"
  );

  const allOthers = otherBips.reduce<Reward[]>(
    (accum, curr) => concat(accum, defaultTo(props.selected[curr], [])),
    []
  );

  let boostEarned = (
    <Text
      style={[TextStyles.fontWeight600, { fontSize: 28, color: "#FFFFFF" }]}
    >
      <Text style={[{ fontSize: 16 }]}>$</Text>
      {props.balance.toFixed(2)}
    </Text>
  );
  if (props.balance === Infinity) {
    boostEarned = (
      <ActivityIndicator
        style={{ marginTop: 10 }}
        color="#FFFFFF"
        size="small"
      />
    );
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
            { paddingTop: top + (Platform.OS === "ios" ? 17 : 24) },
            {
              backgroundColor: "#2A82D4",
              flexDirection: "column",
              alignItems: "center",
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
          <Text
            style={[
              TextStyles.fontWeight600,
              { fontSize: 14, color: "#FFFFFF" },
            ]}
          >
            Available rewards
          </Text>
          {boostEarned}
        </View>
      </View>

      <ScrollView style={[{ flex: 1 }]}>
        {allOthers.map((reward) => (
          <React.Fragment key={reward.boostId}>
            <View style={[styles.intermodulePadding]} />
            <View style={[styles.bubble]}>
              <LinearGradient
                style={[
                  StyleSheet.absoluteFill,
                  {
                    borderRadius: 10,
                    overflow: "hidden",
                  },
                ]}
                colors={["#B779F6", "#C765D1"]}
                start={{ x: 0.5, y: 0.0 }}
                end={{ x: 0.5, y: 1.0 }}
              />
              <View style={[styles.header]}>
                <Text
                  style={[
                    TextStyles.fontWeight600,
                    styles.headerText,
                    { fontSize: 26 },
                  ]}
                >
                  {truncate(reward.boostName, {
                    length: 17,
                    omission: "...",
                  })}
                </Text>
                <View
                  style={{
                    width: 44,
                    height: 44,
                    backgroundColor: "#FFFFFF",
                    borderRadius: 999,
                    justifyContent: "center",
                    alignItems: "center",
                  }}
                >
                  <Text
                    style={[
                      TextStyles.fontWeight600,
                      { color: "#B779F6", fontSize: 20, marginLeft: 4 },
                    ]}
                  >
                    {reward.boostRewardInBips / 100}
                    <Text style={[TextStyles.fontWeight500, { fontSize: 10 }]}>
                      %
                    </Text>
                  </Text>
                </View>
              </View>
              {reward.boostUses === null ? (
                <Text style={[TextStyles.fontWeight400, styles.explainerText]}>
                  You're getting {reward.boostRewardInBips / 100}% cash back on
                  all purchases for a limited time
                </Text>
              ) : (
                <Text style={[TextStyles.fontWeight400, styles.explainerText]}>
                  You're getting {reward.boostRewardInBips / 100}% cash back on{" "}
                  {reward.boostUses === 1
                    ? "1 purchase"
                    : `the next ${reward.boostUses} purchases`}
                </Text>
              )}
            </View>
          </React.Fragment>
        ))}

        <View style={[styles.intermodulePadding]} />
        <View
          style={[
            {
              backgroundColor: "#F65E93",
            },
            styles.bubble,
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
          <Pressable
            style={[styles.header]}
            onPressIn={() => setFivePressed(true)}
            onPressOut={() => setFivePressed(false)}
            onPress={() => props.gotoActivator("500")}
          >
            <Text style={[TextStyles.fontWeight600, styles.headerText]}>
              Date night
            </Text>
            <FivePercent width={44} height={44} />
          </Pressable>
          <Text style={[TextStyles.fontWeight400, styles.explainerText]}>
            Pick 2 boosts to make your nights out (or in) more exciting, or at
            least 5% cheaper
          </Text>
          <Pressable
            style={[{ flexDirection: "row", alignItems: "center" }]}
            onPressIn={() => setFivePressed(true)}
            onPressOut={() => setFivePressed(false)}
            onPress={() => props.gotoActivator("500")}
          >
            {fivePressed ? (
              <DashedCircle width={44} height={44} />
            ) : (
              <BoostIcon boost={selected500[0]} />
            )}
            <View style={[{ width: 8 }]} />
            {fivePressed ? (
              <DashedCircle width={44} height={44} />
            ) : (
              <BoostIcon boost={selected500[1]} />
            )}
            <Text
              style={[
                TextStyles.fontWeight600,
                styles.boostsAvailableText,
                { opacity: fivePressed ? 0.5 : 1.0 },
              ]}
            >
              {2 - selected500.length === 0
                ? "Tap here to change boosts"
                : 2 - selected500.length === 1
                ? "1 boost available"
                : `${2 - selected500.length} boosts available`}
            </Text>
          </Pressable>
        </View>

        <View style={[styles.intermodulePadding]} />
        <View
          style={[
            {
              backgroundColor: "#3AD79E",
            },
            styles.bubble,
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
            colors={["#3AD79E", "#36B2B9"]}
            start={{ x: 0.5, y: 0.0 }}
            end={{ x: 0.5, y: 1.0 }}
          />
          <Pressable
            style={[styles.header]}
            onPressIn={() => setThreePressed(true)}
            onPressOut={() => setThreePressed(false)}
            onPress={() => props.gotoActivator("300")}
          >
            <Text style={[TextStyles.fontWeight600, styles.headerText]}>
              Home life
            </Text>
            <ThreePercent width={44} height={44} />
          </Pressable>
          <Text style={[TextStyles.fontWeight400, styles.explainerText]}>
            Pick 2 boosts to help you save on day-to-day household expenses
          </Text>
          <Pressable
            style={[{ flexDirection: "row", alignItems: "center" }]}
            onPressIn={() => setThreePressed(true)}
            onPressOut={() => setThreePressed(false)}
            onPress={() => props.gotoActivator("300")}
          >
            {threePressed ? (
              <DashedCircle width={44} height={44} />
            ) : (
              <BoostIcon boost={selected300[0]} />
            )}
            <View style={[{ width: 8 }]} />
            {threePressed ? (
              <DashedCircle width={44} height={44} />
            ) : (
              <BoostIcon boost={selected300[1]} />
            )}
            <Text
              style={[
                TextStyles.fontWeight600,
                styles.boostsAvailableText,
                { opacity: threePressed ? 0.5 : 1.0 },
              ]}
            >
              {2 - selected300.length === 0
                ? "Tap here to change boosts"
                : 2 - selected300.length === 1
                ? "1 boost available"
                : `${2 - selected300.length} boosts available`}
            </Text>
          </Pressable>
        </View>

        <View style={[styles.intermodulePadding]} />
        <View style={[styles.bubble]}>
          <LinearGradient
            style={[
              StyleSheet.absoluteFill,
              {
                borderRadius: 10,
                overflow: "hidden",
              },
            ]}
            colors={["#2A82D4", "#49A1E0"]}
            start={{ x: 0.5, y: 0.0 }}
            end={{ x: 0.5, y: 1.0 }}
          />
          <View style={[styles.header]}>
            <Text style={[TextStyles.fontWeight600, styles.headerText]}>
              Everything else
            </Text>
            <OnePercent width={44} height={44} />
          </View>
          <Text style={[TextStyles.fontWeight400, styles.explainerText]}>
            Get 1% cashback on anything else
          </Text>
        </View>

        <View style={[{ height: 40 }]} />
        <View style={[styles.padding]}>
          <Pressable
            style={[styles.balancebutton]}
            onPressIn={() => setTransferPressed(true)}
            onPressOut={() => setTransferPressed(false)}
            onPress={() => props.transfer(props.balance)}
          >
            {props.transferInProgrss ? (
              <ActivityIndicator size="small" color="#FFFFFF" />
            ) : (
              <Text
                style={[
                  TextStyles.fontWeight600,
                  styles.balanceButtonText,
                  {
                    opacity:
                      transferPressed || props.balance < 10.0 ? 0.5 : 1.0,
                  },
                ]}
              >
                {`Apply $${props.balance
                  .toFixed(2)
                  .replace(".00", "")} to Tgthr Card`}
              </Text>
            )}
          </Pressable>
        </View>

        <View style={[{ height: 40 }]} />
        {props.entries.map((e) => (
          <View
            key={e.revision}
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
                    style={[StyleSheet.absoluteFill]}
                    colors={
                      defaultTo(db[e.reward || ""]?.boostRewardInBips, 100) /
                        100 ===
                      5
                        ? ["#F65E93", "#FD7776"]
                        : defaultTo(
                            db[e.reward || ""]?.boostRewardInBips,
                            100
                          ) /
                            100 ===
                          3
                        ? ["#3AD79E", "#36B2B9"]
                        : ["#2A82D4", "#49A1E0"]
                    }
                  />
                  <Text
                    style={[
                      TextStyles.fontWeight600,
                      theme.textStyle,
                      {
                        fontSize: 18,
                        color: "#FFFFFF",
                        marginLeft: 4,
                        marginTop: -2,
                      },
                    ]}
                  >
                    {e.reward === null
                      ? "1"
                      : `${
                          defaultTo(
                            db[e.reward || ""]?.boostRewardInBips,
                            100
                          ) / 100
                        }`}
                    <Text style={[TextStyles.fontWeight400, { fontSize: 12 }]}>
                      %
                    </Text>
                  </Text>
                </View>
                <View style={[{ marginLeft: 12, flex: 1 }]}>
                  <Text
                    style={[
                      TextStyles.fontWeight600,
                      theme.textStyle,
                      { fontSize: 16 },
                    ]}
                  >
                    {e.description}
                  </Text>
                </View>
              </View>
              <View>
                <Text
                  style={[
                    TextStyles.fontWeight600,
                    theme.textStyle,
                    { fontSize: 15 },
                  ]}
                >
                  ${(e.amount[1] / e.amount[2]).toFixed(2)}
                </Text>
              </View>
            </View>
          </View>
        ))}
      </ScrollView>
    </View>
  );
}

const styles = StyleSheet.create({
  balanceButtonText: { color: "#FFFFFF", fontSize: 16 },
  balancebutton: {
    alignItems: "center",
    backgroundColor: "#EA4352",
    borderRadius: 999,
    height: 50,
    justifyContent: "center",
    width: "100%",
  },
  boostsAvailableText: {
    color: "#FFFFFF",
    flex: 1,
    fontSize: 13,
    marginLeft: 12,
  },
  bubble: {
    backgroundColor: "#49A1E0",
    borderRadius: 10,
    marginHorizontal: 28,
    padding: 16,
  },
  explainerText: {
    color: "#FFFFFF",
    fontSize: 13,
    marginBottom: 16,
    marginTop: 16,
  },
  header: {
    alignItems: "center",
    flexDirection: "row",
    justifyContent: "space-between",
  },
  headerText: { color: "#FFFFFF", fontSize: 28 },
  intermodulePadding: { height: 24 },
  padding: { paddingHorizontal: 28 },
});
