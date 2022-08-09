/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */
import React from "react";
import { StyleSheet, Text, View } from "react-native";
import { LinearGradient } from "expo-linear-gradient";
import { PurchaseProps } from "./Types";

import TextStyles from "../../Styling/Text";

interface Props {
  index: number;
  item: PurchaseProps;
}

export const colors: [string, string][] = [
  ["#F65E93", "#FD7776"],
  ["#2AD4BA", "#49E0D1"],
  ["#F6A75E", "#FD7E76"],
  ["#2A82D4", "#49A1E0"],
  ["#FF5BAF", "#F369BC"],
  ["#38C860", "#36C220"],
  ["#5ED2F6", "#30B0F9"],
];

export default function Purchase({ index, item }: Props): JSX.Element {
  const myColor = colors[index % colors.length];
  let StatePill = () => <></>;

  if (item.isDeclined) {
    StatePill = function Pill() {
      return (
        <View style={[styles.pill]}>
          <Text
            style={[
              TextStyles.fontWeight600,
              styles.pillText,
              { color: "red" },
            ]}
          >
            Declined
          </Text>
        </View>
      );
    };
  }

  return (
    <View
      style={[
        {
          paddingRight: index % 2 === 0 ? 19 / 2 : 28,
          paddingLeft: index % 2 === 1 ? 19 / 2 : 28,
        },
        styles.purchase,
      ]}
    >
      <View style={[styles.card]}>
        <LinearGradient
          style={[StyleSheet.absoluteFill, { backgroundColor: myColor[0] }]}
          colors={myColor}
        />
        <View style={[styles.emoji]}>
          <Text style={[{ fontSize: 15 }]}>
            {String.fromCodePoint(parseInt(item.mccEmoji.slice(3, -1), 16))}
          </Text>
        </View>
        <View>
          <Text
            style={[
              TextStyles.fontWeight600,
              { color: "white", textAlign: "center" },
            ]}
          >
            {item.description
              .replace("  ", " ")
              .replace("# ", "#")
              .replace("SQ *", "")
              .replace("UBR*", "")}
          </Text>
        </View>
        <View style={[{ justifyContent: "center", alignItems: "center" }]}>
          <Text
            style={[
              TextStyles.fontWeight600,
              { color: "white", fontSize: 24, marginTop: -3 },
            ]}
          >
            <Text style={[{ fontSize: 18, color: "#FFFFFFFF" }]}>$</Text>
            {item.amount.replace(".00", "")}
          </Text>
          <Text
            style={[
              TextStyles.fontWeight500,
              { color: "white", fontSize: 13, marginTop: -5 },
            ]}
          >
            {item.purchasedAt}
          </Text>
        </View>
        {item.isDeclined ? (
          <></>
        ) : (
          <View style={[{ justifyContent: "center", alignItems: "center" }]}>
            <View
              style={[
                {
                  backgroundColor: "#FFFFFF70",
                  paddingHorizontal: 10,
                  paddingVertical: 5,
                  borderRadius: 999,
                },
              ]}
            >
              <Text
                style={[
                  TextStyles.fontWeight600,
                  { color: "white", fontSize: 12 },
                ]}
              >
                {item.userRatio}:{item.partnerRatio}
              </Text>
            </View>

            <Text
              style={[
                TextStyles.fontWeight500,
                { color: "white", fontSize: 13 },
              ]}
            >
              Your share{" "}
              <Text style={[TextStyles.fontWeight600]}>
                ${item.userShare.replace(".00", "")}
              </Text>
            </Text>
          </View>
        )}
        <StatePill />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  card: {
    alignItems: "center",
    aspectRatio: 150 / 220,
    backgroundColor: "#00FF00",
    borderRadius: 16,
    flex: 1,
    flexDirection: "column",
    justifyContent: "space-between",
    overflow: "hidden",
    paddingHorizontal: 10,
    paddingVertical: 12,
    position: "relative",
  },
  emoji: {
    alignItems: "center",
    backgroundColor: "white",
    borderRadius: 32,
    height: 32,
    justifyContent: "center",
    width: 32,
  },
  pill: {
    backgroundColor: "#FFFFFF",
    borderRadius: 999,
    paddingHorizontal: 12,
    paddingVertical: 5,
  },
  pillText: { fontSize: 11 },
  purchase: {
    overflow: "hidden",
    position: "relative",
    width: "50%",
  },
});
