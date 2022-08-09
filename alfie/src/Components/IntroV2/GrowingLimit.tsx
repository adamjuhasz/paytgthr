import React, { useContext } from "react";
import { Image, StatusBar, StyleSheet, Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import IntroContainer from "./IntroContainer";
import IntroBottom, { styles as BottomStyles } from "./IntroBottom";
import Button from "../Button/Button";
import { gradients } from "../Styling/Colors";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

const Img = require("../../../assets/IntroV2/GrowingLimit/Artboard.png");

interface Props {
  pageIndex: number;
  pageCount: number;
  next: () => void;
}

const GrowingLimit = ({ next, ...props }: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const insets = useSafeAreaInsets();

  return (
    <IntroContainer
      gradient={gradients.pinkhaze}
      background={
        <View style={[StyleSheet.absoluteFill, { top: insets.top }]}>
          <StatusBar hidden />
          <Image source={Img} resizeMode="contain" style={[styles.fullImage]} />
        </View>
      }
      {...props}
    >
      <IntroBottom
        title="A spending limit that grows with you"
        body={
          <>
            <Text
              style={[
                TextStyles.fontWeight400,
                TextStyles.centered,
                theme.textStyle,
                BottomStyles.body,
              ]}
            >
              Every time you use the Tgthr Card and pay us back, we'll bump up
              your spending limit
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                TextStyles.centered,
                theme.textStyle,
                BottomStyles.body,
              ]}
            >
              Start at $150 per day and grow to $2000
            </Text>
          </>
        }
        buttons={
          <>
            <Button style="Primary" text="Next" onPress={next} />
          </>
        }
      />
    </IntroContainer>
  );
};

export default GrowingLimit;

const styles = StyleSheet.create({
  fullImage: { height: "100%", width: "100%" },
});
