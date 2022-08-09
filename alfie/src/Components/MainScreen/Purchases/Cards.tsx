/* eslint-disable react-native/no-inline-styles */
import React, { useContext } from "react";
import { ActivityIndicator, FlatList, StyleSheet, View } from "react-native";

import { EnhancedCardModel } from "../../../Actions/Card/Types";

import Card from "./Card";
import { ThemeContext } from "../../Theming/ThemeContext";

interface Props {
  cards: EnhancedCardModel[];
  width: number;
  copyToClipboard: (
    notification: string,
    copiable: string,
    event: string
  ) => void;
  locked: boolean;
  activateCard: (cardId: string) => void;
  setPin: (cardId: string) => void;
  lockCard: (cardId: string) => void;
  unlockCard: (cardId: string) => void;
}

export default function Cards(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);
  return (
    <View
      style={[
        { height: (props.width - 28 * 2) * (203 / 319) + 40 * 2 },
        styles.header,
      ]}
    >
      <FlatList
        horizontal
        alwaysBounceHorizontal={false}
        keyExtractor={(item) => item.cardId}
        data={props.cards}
        showsHorizontalScrollIndicator={false}
        ListHeaderComponent={() => <View style={[styles.cardPadding]} />}
        ListFooterComponent={() => <View style={[styles.cardPadding]} />}
        ItemSeparatorComponent={() => <View style={[{ width: 28 / 2 }]} />}
        renderItem={(p) => (
          <View style={[styles.card]}>
            <Card
              {...p}
              copyToClipboard={props.copyToClipboard}
              locked={props.locked}
              activateCard={props.activateCard}
              lockCard={props.lockCard}
              unlockCard={props.unlockCard}
              setPin={props.setPin}
            />
          </View>
        )}
        ListEmptyComponent={() => (
          <View
            style={[
              {
                width: props.width - 28 * 2,
              },
              styles.activity,
            ]}
          >
            <ActivityIndicator color={theme.textColor} />
          </View>
        )}
      />
    </View>
  );
}

const styles = StyleSheet.create({
  activity: {
    alignItems: "center",
    flex: 1,
    justifyContent: "center",
  },
  card: { paddingVertical: 40 },
  cardPadding: { width: 28 },
  header: {
    marginTop: -20,
    width: "100%",
  },
});
