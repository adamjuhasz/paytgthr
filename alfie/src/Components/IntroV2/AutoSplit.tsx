import React from "react";
import { Image, StatusBar, StyleSheet, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import IntroContainer from "./IntroContainer";
import IntroBottom from "./IntroBottom";
import Button from "../Button/Button";
import { gradients } from "../Styling/Colors";

const WiggleImg = require("../../../assets/IntroV2/AutoSplit/Artboard.png");

interface Props {
  pageIndex: number;
  pageCount: number;
  next: () => void;
}

const AutoSplit = ({ next, ...props }: Props): JSX.Element => {
  const insets = useSafeAreaInsets();
  return (
    <IntroContainer
      gradient={gradients.sunset}
      background={
        <View style={[StyleSheet.absoluteFill, { top: insets.top }]}>
          <StatusBar hidden />
          <Image
            source={WiggleImg}
            style={[styles.fullImage]}
            resizeMode="contain"
          />
        </View>
      }
      {...props}
    >
      <IntroBottom
        title="Automatically split purchases"
        body="The Tgthr Card is a new type of Mastercard that splits purchases between the checking accounts you and your partner already have"
        buttons={
          <>
            <Button style="Primary" text="Next" onPress={next} />
          </>
        }
      />
    </IntroContainer>
  );
};

export default AutoSplit;

const styles = StyleSheet.create({
  fullImage: { height: "100%", width: "100%" },
});
