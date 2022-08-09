/* eslint-disable react-native/no-color-literals */
import React, { useContext, useState } from "react";
import { Pressable, StyleSheet, Text, View } from "react-native";
import { EnhancedCardModel } from "../../../Actions/Card/Types";
import { LinearGradient } from "expo-linear-gradient";

import CardChip from "./CardChip";
import Mastercard from "./Mastercard";

import TextStyles from "../../Styling/Text";
import { pinkColor } from "../../Styling/Colors";
import { ThemeContext } from "../../Theming/ThemeContext";
import {
  connectActionSheet,
  useActionSheet,
} from "@expo/react-native-action-sheet";

interface Props {
  index: number;
  item: EnhancedCardModel;
  copyToClipboard: (
    notification: string,
    copiable: string,
    event: string
  ) => void;
  locked?: boolean;
  activateCard: (cardId: string) => void;
  setPin: (cardId: string) => void;
  lockCard: (cardId: string) => void;
  unlockCard: (cardId: string) => void;
}

const splitPan = (s: string): string => {
  const splitPAN = s.match(/.{4}/g);
  const panDisplay = splitPAN === null ? s : splitPAN.join("   ");
  return panDisplay;
};

const hideString = (hide: boolean, s: string): string => {
  if (hide) {
    return `${s.replace(/(\d)/g, "●")} `;
  } else {
    return s;
  }
};

const Card = ({
  item,
  copyToClipboard,
  locked = false,
  activateCard,
  setPin,
  lockCard,
  unlockCard,
}: Props): JSX.Element => {
  const [panPressed, setPanPress] = useState(false);
  const [expPressed, setExpPress] = useState(false);
  const [cvvPressed, setCvvPress] = useState(false);
  const [cardPressed, setCardPress] = useState(false);
  const theme = useContext(ThemeContext);
  const { showActionSheetWithOptions } = useActionSheet();

  let cardPressable = false;
  let pressAction: typeof activateCard = (_cardId: string) => {
    return;
  };
  let Banner = () => <></>;
  let hidePan = locked;

  switch (item.cardStatus) {
    case "CLOSED":
      hidePan = true;
      Banner = function BannerClosed() {
        return (
          <View style={[styles.stateBanner]}>
            <Text style={[TextStyles.fontWeight400, styles.stateBannerText]}>
              Closed
            </Text>
          </View>
        );
      };
      break;

    case "ADMINFROZEN":
      hidePan = true;
      Banner = function BannerClosed() {
        return (
          <View style={[styles.stateBanner]}>
            <Text style={[TextStyles.fontWeight400, styles.stateBannerText]}>
              Frozen
            </Text>
          </View>
        );
      };
      break;

    case "USERFROZEN":
      hidePan = true;
      Banner = function BannerClosed() {
        return (
          <View style={[styles.stateBanner]}>
            <Text style={[TextStyles.fontWeight600, styles.stateBannerText]}>
              Frozen
            </Text>
          </View>
        );
      };
      break;

    case "ACTIVATED":
      break;

    case "CREATED":
      switch (item.cardDesign) {
        case "PhysicalBlack":
          hidePan = true;
          Banner = function BannerClosed() {
            return (
              <View style={[styles.happyBanner]}>
                <LinearGradient
                  colors={["#30B0F9", "#5ED2F6"]}
                  start={{ x: 0, y: 0.5 }}
                  end={{ x: 1, y: 0.5 }}
                  style={[StyleSheet.absoluteFill]}
                />
                <Text
                  style={[TextStyles.fontWeight600, styles.stateBannerText]}
                >
                  Tap to activate
                </Text>
              </View>
            );
          };
          break;
        case "DigitalWallet":
        case "pink":
        case "yellow":
        case "virtual":
          break;
      }
      break;
  }

  switch (item.cardStatus) {
    case "CLOSED":
      cardPressable = false;
      break;

    case "CREATED":
      cardPressable = true;
      pressAction = activateCard;
      break;

    case "ACTIVATED":
      switch (item.cardDesign) {
        case "PhysicalBlack":
          cardPressable = true;
          pressAction = (cardid: string) =>
            showActionSheetWithOptions(
              {
                // title: "Card options",
                // message: ``,
                options: [
                  "Cancel", // 0
                  "Set PIN for card", // 1
                  "Lock card", // 2
                ],
                cancelButtonIndex: 0,
              },
              (index) => {
                switch (index) {
                  case 0:
                    // close
                    return;

                  case 1:
                    setPin(cardid);
                    return;

                  case 2:
                    lockCard(cardid);
                    return;
                }
              }
            );
          break;

        case "DigitalWallet":
        case "virtual":
          cardPressable = true;
          pressAction = (cardid: string) =>
            showActionSheetWithOptions(
              {
                // title: "Card options",
                // message: ``,
                options: [
                  "Cancel", // 0
                  "Lock card", // 1
                ],
                cancelButtonIndex: 0,
              },
              (index) => {
                switch (index) {
                  case 0:
                    // close
                    return;

                  case 1:
                    lockCard(cardid);
                    return;
                }
              }
            );
          break;

        default:
          break;
      }
      break;

    case "USERFROZEN":
      cardPressable = true;
      pressAction = (cardid: string) =>
        showActionSheetWithOptions(
          {
            // title: "Card options",
            // message: ``,
            options: [
              "Cancel", // 0
              "Unlock card", // 1
            ],
            cancelButtonIndex: 0,
          },
          (index) => {
            switch (index) {
              case 0:
                // close
                return;

              case 1:
                unlockCard(cardid);
                return;
            }
          }
        );
      break;

    case "ADMINFROZEN":
      cardPressable = false;
  }

  if (locked) {
    hidePan = true;
    cardPressable = false;
    Banner = function BannerClosed() {
      return (
        <View style={[styles.stateBanner]}>
          <Text style={[TextStyles.fontWeight600, styles.stateBannerText]}>
            Pending
          </Text>
        </View>
      );
    };
  }

  let cardType = "Virtual";
  switch (item.cardDesign) {
    case "DigitalWallet":
    case "virtual":
      cardType = "Virtual";
      break;

    case "pink":
    case "yellow":
    case "PhysicalBlack":
      cardType = "Physical";
      break;
  }

  return (
    <Pressable
      disabled={!cardPressable}
      onPressIn={() => setCardPress(true)}
      onPressOut={() => setCardPress(false)}
      onPress={() => pressAction(item.cardId)}
      style={[
        { backgroundColor: theme.textColor },
        styles.card,
        cardPressed ? {} : styles.cardShadow,
        cardPressed ? { transform: [{ scale: 0.95 }] } : {},
      ]}
    >
      <CardChip
        backgroundColor="#DADADA"
        foregroundColor="#B2B2B2"
        style={[styles.chip]}
      />
      <Mastercard
        leftColor="#C1C1C1"
        rightColor="#DADADA"
        middlecolor="#B2B2B2"
        style={[styles.mastercard]}
      />
      <Banner />
      <Text
        style={[
          TextStyles.nexaHeavy,
          { color: theme.backgroundColor },
          styles.logo,
        ]}
      >
        {cardType}
      </Text>

      <Pressable
        onPressIn={() => setPanPress(true)}
        onPressOut={() => setPanPress(false)}
        onPress={() => {
          copyToClipboard(
            "Card number copied",
            hidePan ? "**** **** **** ****" : item.pan,
            "Pan"
          );
        }}
      >
        <Text
          style={[
            TextStyles.fontWeight600,
            { color: panPressed ? pinkColor : theme.backgroundColor },
            styles.pan,
          ]}
        >
          {hideString(hidePan, splitPan(item.pan))}
        </Text>
      </Pressable>
      <Pressable
        onPressIn={() => setExpPress(true)}
        onPressOut={() => setExpPress(false)}
        onPress={() => {
          copyToClipboard(
            "Expiration copied",
            hidePan ? "**/**" : `${item.expMonth}/${item.expShortYear}`,
            "Exp"
          );
        }}
      >
        <View style={[styles.exp]}>
          <Text
            style={[
              TextStyles.fontWeight600,
              styles.pciSmall,
              {
                color: expPressed ? pinkColor : theme.backgroundColor,
              },
            ]}
          >
            {hideString(hidePan, `${item.expMonth} / ${item.expShortYear}`)}
          </Text>
          <Text
            style={[
              TextStyles.fontWeight400,
              styles.pciTag,
              {
                color: theme.backgroundColor,
              },
            ]}
          >
            EXP
          </Text>
        </View>
      </Pressable>
      <Pressable
        onPressIn={() => setCvvPress(true)}
        onPressOut={() => setCvvPress(false)}
        onPress={() => {
          copyToClipboard("CVV copied", hidePan ? "***" : item.cvv, "CVV");
        }}
      >
        <View style={[styles.cvv]}>
          <Text
            style={[
              TextStyles.fontWeight600,
              styles.pciSmall,
              {
                color: cvvPressed ? pinkColor : theme.backgroundColor,
              },
            ]}
          >
            {hideString(hidePan, item.cvv)}
          </Text>
          <Text
            style={[
              TextStyles.fontWeight400,
              styles.pciTag,
              {
                color: theme.backgroundColor,
              },
            ]}
          >
            CVV
          </Text>
        </View>
      </Pressable>
    </Pressable>
  );
};

export default connectActionSheet(Card);

const styles = StyleSheet.create({
  card: {
    aspectRatio: 319 / 203,
    borderColor: "#3E3E3E",
    borderRadius: 16,
    borderWidth: 1,
    flex: 1,
  },
  cardShadow: {
    elevation: 5,
    shadowColor: "#000",
    shadowOffset: {
      width: 0,
      height: 2,
    },
    shadowOpacity: 0.25,
    shadowRadius: 3.84,
  },
  chip: { left: 29, position: "absolute", top: 61 },
  cvv: {
    left: 120,
    position: "absolute",
    top: 155,
  },
  exp: {
    left: 29,
    position: "absolute",
    top: 155,
  },
  happyBanner: {
    backgroundColor: "#30B0F9",
    borderRadius: 999,
    left: 15,
    overflow: "hidden",
    paddingHorizontal: 4,
    position: "absolute",
    top: 15,
  },
  logo: {
    fontSize: 22,
    position: "absolute",
    right: 22,
    top: 21,
  },
  mastercard: { bottom: 16, position: "absolute", right: 22 },
  pan: {
    fontSize: 17,
    left: 29,
    position: "absolute",
    top: 105,
  },
  pciSmall: {
    fontSize: 15,
  },
  pciTag: {
    fontSize: 12,
    lineHeight: 15,
  },
  stateBanner: {
    backgroundColor: "red",
    borderRadius: 3,
    left: 15,
    position: "absolute",
    top: 15,
  },
  stateBannerText: { color: "white", paddingHorizontal: 8, paddingVertical: 5 },
});
