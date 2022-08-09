import React, { useContext } from "react";
import { Image, StatusBar, StyleSheet, Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import IntroContainer from "./IntroContainer";
import IntroBottom, { styles as BottomStyles } from "./IntroBottom";
import Button from "../Button/Button";
import { gradients } from "../Styling/Colors";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

const Img = require("../../../assets/IntroV2/NotChecking/Artboard.png");

interface Props {
  pageIndex: number;
  pageCount: number;
  next: () => void;
}

const NotChecking = ({ next, ...props }: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const insets = useSafeAreaInsets();

  return (
    <IntroContainer
      gradient={gradients.hawaii}
      background={
        <View
          style={[StyleSheet.absoluteFill, styles.image, { top: insets.top }]}
        >
          <StatusBar hidden />
          <Image source={Img} resizeMode="contain" style={[styles.fullImage]} />
        </View>
      }
      {...props}
    >
      <IntroBottom
        title="Nothing to load"
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
              It's <Text style={[TextStyles.fontWeight600]}>not</Text> a
              checking account
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                TextStyles.centered,
                theme.textStyle,
                BottomStyles.body,
                styles.noMargin,
              ]}
            >
              It's <Text style={[TextStyles.fontWeight600]}>not</Text> a prepaid
              card
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                TextStyles.centered,
                theme.textStyle,
                BottomStyles.body,
              ]}
            >
              We charge you and your partner after each purchase, keep your
              money where it is today
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

export default NotChecking;

const styles = StyleSheet.create({
  fullImage: { height: "100%", width: "100%" },
  image: { bottom: 20 },
  noMargin: { marginTop: 0 },
});
