import React, { useContext } from "react";
import { Image, StatusBar, StyleSheet, Text, View } from "react-native";

import IntroContainer from "./IntroContainer";
import IntroBottom, {
  styles as BottomStyles,
  TitleFontSize,
} from "./IntroBottom";
import Button from "../Button/Button";
import { gradients } from "../Styling/Colors";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

const Img = require("../../../assets/IntroV2/Pricing/Artboard.png");

interface Props {
  pageIndex: number;
  pageCount: number;
  next: () => void;
}

const Pricing = ({ next, ...props }: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  const handTune = 5;

  return (
    <IntroContainer
      gradient={gradients.deepocean}
      background={
        <View style={[StyleSheet.absoluteFill]}>
          <StatusBar hidden />
          <Image source={Img} resizeMode="contain" style={[styles.fullImage]} />
        </View>
      }
      {...props}
    >
      <IntroBottom
        title={
          <Text
            style={[
              TextStyles.nexaHeavy,
              TextStyles.centered,
              BottomStyles.title,
              theme.textStyle,
              {
                lineHeight: TitleFontSize + handTune,
                marginBottom: -1 * handTune,
              },
            ]}
          >
            Just $1 a month
          </Text>
        }
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
              {`Cheaper than adding guac to\nyour burrito 🥑`}
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                TextStyles.centered,
                theme.textStyle,
                BottomStyles.body,
              ]}
            >
              Not sure about us yet?
            </Text>
            <Text
              style={[
                TextStyles.fontWeight600,
                TextStyles.centered,
                theme.textStyle,
                BottomStyles.body,
                styles.noMargin,
              ]}
            >
              The first 2 months are free!
            </Text>
          </>
        }
        buttons={
          <>
            <Button style="Primary" text="Done" onPress={next} />
          </>
        }
      />
    </IntroContainer>
  );
};

export default Pricing;

const styles = StyleSheet.create({
  fullImage: { height: "100%", width: "100%" },
  noMargin: { marginTop: 0 },
});
