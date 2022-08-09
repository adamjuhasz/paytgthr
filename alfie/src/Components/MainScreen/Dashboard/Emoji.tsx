import React from "react";
import { StyleSheet, Text, View } from "react-native";

import { IconProps } from "../../Icons/Types";

interface EmojiProps {
  emoji: string;
  color: string;
}

const Emoji = (props: IconProps & EmojiProps): JSX.Element => (
  <View style={[emojiStyles.pill, { backgroundColor: props.color }]}>
    <Text style={[emojiStyles.font]}>{props.emoji}</Text>
  </View>
);

const emojiStyles = StyleSheet.create({
  font: {
    fontSize: 18,
  },
  pill: {
    alignItems: "center",
    aspectRatio: 1,
    borderRadius: 40,
    flexDirection: "row",
    justifyContent: "center",
    overflow: "hidden",
    padding: 8,
  },
});

export default Emoji;
