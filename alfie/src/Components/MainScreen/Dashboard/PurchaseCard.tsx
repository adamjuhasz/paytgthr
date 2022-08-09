/* eslint-disable react-native/no-inline-styles */

import React from "react";
import {
  ListRenderItemInfo,
  Platform,
  StyleSheet,
  Text,
  TouchableOpacity,
  View,
} from "react-native";
import { LinearGradient } from "expo-linear-gradient";
import { join, split, upperFirst } from "lodash";

import TextStyles from "../../Styling/Text";
import { whiteColor } from "../../Styling/Colors";

import { colors } from "../Purchases/Purchase";
import { PurchaseProps } from "../Purchases/Types";

interface Props {
  gotoPurhcase: (id: string) => void;
}

const PurchaseCard = (
  info: ListRenderItemInfo<PurchaseProps> & Props
): JSX.Element => {
  const gradientColors = colors[info.index % colors.length];
  return (
    <View style={[styles.container, { opacity: info.item.isFake ? 0.5 : 1.0 }]}>
      <TouchableOpacity onPress={() => info.gotoPurhcase(info.item.id)}>
        <LinearGradient
          style={[
            styles.gradient,
            {
              backgroundColor: gradientColors[1],
            },
          ]}
          start={{ x: 1.0, y: 0 }}
          end={{ x: 0.5, y: 0.7 }}
          colors={gradientColors}
        >
          <View
            style={[
              styles.emojiContainer,
              { opacity: info.item.isFake ? 0.5 : 1.0 },
            ]}
          >
            <Text style={[TextStyles.fontWeight400, styles.emojiText]}>
              {String.fromCodePoint(
                parseInt(info.item.mccEmoji.slice(3, -1), 16)
              )}
            </Text>
          </View>
          <View
            style={[
              styles.descriptionContainer,
              { opacity: info.item.isFake ? 1.0 : 1.0 },
            ]}
          >
            <Text
              numberOfLines={1}
              ellipsizeMode="tail"
              style={[TextStyles.fontWeight500, styles.descriptionText]}
            >
              {join(
                split(info.item.description.toLowerCase(), " ").map(upperFirst),
                " "
              )}
            </Text>
          </View>
          <View
            style={[
              styles.amountContainer,
              { opacity: info.item.isFake ? 1.0 : 1.0 },
            ]}
          >
            <Text
              numberOfLines={1}
              style={[TextStyles.fontWeight600, styles.amountText]}
            >
              ${info.item.amount}
            </Text>
          </View>
        </LinearGradient>
      </TouchableOpacity>
    </View>
  );
};

export default PurchaseCard;

const styles = StyleSheet.create({
  amountContainer: {
    marginTop: 0,
    paddingHorizontal: 10,
  },
  amountText: { color: whiteColor, fontSize: 16 },
  container: { paddingBottom: 10 },
  descriptionContainer: {
    marginTop: 5,
    paddingHorizontal: 10,
  },
  descriptionText: { color: whiteColor, fontSize: 14 },
  emojiContainer: {
    alignItems: "center",
    aspectRatio: 1,
    backgroundColor: whiteColor,
    borderRadius: 27.5,
    justifyContent: "center",
    padding: 7,
  },
  emojiText: { fontSize: 14 },
  gradient: {
    alignItems: "center",
    aspectRatio: Platform.OS !== "web" ? 450 / 300 : undefined,
    borderRadius: 12,
    height: 100,
    justifyContent: "center",
    width: Platform.OS === "web" ? 100 * (450 / 300) : undefined,
  },
});
