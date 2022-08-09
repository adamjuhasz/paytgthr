/* eslint-disable react-native/no-inline-styles */
import React, { useContext } from "react";
import {
  FlatList,
  StyleSheet,
  Text,
  View,
  useWindowDimensions,
} from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { PurchaseProps } from "./Types";
import { EnhancedCardModel } from "../../../Actions/Card/Types";

import Purchase from "./Purchase";
import TextStyles from "../../Styling/Text";
import { ThemeContext } from "../../Theming/ThemeContext";
import Cards from "./Cards";
import Dashboard from "./Dashboard";

interface Props {
  demoMode: boolean;
  purchases: PurchaseProps[];
  cards: EnhancedCardModel[];
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
  canSpend: number;
  maxSpend: number;
}

export default function Purchases(props: Props): JSX.Element {
  const { top } = useSafeAreaInsets();
  const { width } = useWindowDimensions();
  const theme = useContext(ThemeContext);

  return (
    <View style={[StyleSheet.absoluteFill, { paddingTop: top }]}>
      {props.demoMode ? (
        <View style={[styles.demo]}>
          <Text
            style={[
              TextStyles.fontWeight600,
              theme.textStyle,
              styles.demoTitle,
            ]}
          >
            Demo mode
          </Text>
          <Text
            style={[
              TextStyles.fontWeight400,
              theme.textStyle,
              styles.demoExplainer,
            ]}
          >
            Once you make purchases, you'll see them below
          </Text>
        </View>
      ) : (
        <></>
      )}
      <FlatList
        data={props.purchases}
        numColumns={2}
        ItemSeparatorComponent={() => (
          <View style={[styles.purchaseSeperator]} />
        )}
        ListFooterComponent={() => <View style={[styles.purchaseSeperator]} />}
        ListHeaderComponent={() => (
          <>
            <Cards width={width} {...props} />
            <Dashboard {...props} />
          </>
        )}
        renderItem={(p) => <Purchase {...p} />}
      />
    </View>
  );
}

const styles = StyleSheet.create({
  demo: { marginBottom: 20, marginTop: 20, paddingHorizontal: 28 },
  demoExplainer: { fontSize: 15, textAlign: "center" },
  demoTitle: { fontSize: 28, textAlign: "center" },
  purchaseSeperator: { height: 19 },
});
