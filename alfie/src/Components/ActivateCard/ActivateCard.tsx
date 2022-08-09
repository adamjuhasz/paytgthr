/* eslint-disable react-native/no-inline-styles */
import React, { useContext, useEffect, useState } from "react";
import { StyleSheet, Text } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import { ThemeContext } from "../Theming/ThemeContext";
import TextStyles from "../Styling/Text";
import FixedTextInput from "../FixedTextInput/FixedTextInput";

interface Props {
  activateCard: (lastfour: string) => Promise<void>;
  goBack: () => void;
  inProgress: boolean;
}

export default function ActivateCard(props: Props): JSX.Element {
  const [code, setCode] = useState("");
  const [activateError, setError] = useState(false);
  const theme = useContext(ThemeContext);

  useEffect(() => {
    setError(false);
  }, [code]);

  return (
    <FormScreen
      title="Activate card"
      navigation={{ type: "action", action: props.goBack }}
      buttons={
        <>
          <Button
            style="Primary"
            text="Activate"
            disabled={code.length < 4}
            onPress={async () => {
              try {
                await props.activateCard(code);
              } catch (e) {
                setError(true);
              }
            }}
            inProgress={props.inProgress}
          />
        </>
      }
    >
      <Text
        style={[theme.textStyle, TextStyles.fontWeight600, styles.explainer]}
      >
        Enter the last four digits of your physical card's number below
      </Text>
      <FixedTextInput
        placeholder="9876"
        onChangeText={(t) => {
          setCode(t);
        }}
        textInputStyle={{
          ...TextStyles.fontWeight600,
          textTransform: "capitalize",
          ...(activateError === true
            ? { borderColor: "red", borderWidth: 2 }
            : {}),
        }}
      />
    </FormScreen>
  );
}

const styles = StyleSheet.create({
  explainer: { fontSize: 16, marginBottom: 40, paddingHorizontal: 10 },
});
