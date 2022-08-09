import React, { useState } from "react";
import { Text, View } from "react-native";

import CardChoice from "./CardChoice";
import WhiteCard from "./WhiteCard";
import BlackCard from "./BlackCard";

import TextStyles from "../Styling/Text";

const CardPicker = (_props: unknown): JSX.Element => {
  const options = ["white", "black"] as const;
  type aStr = typeof options[number];
  const [selection, setSelection] = useState<null | aStr>(null);

  return (
    <View>
      {options.map((o) => (
        <CardChoice key={o} selected={o === selection} onPress={setSelection}>
          <Text style={[TextStyles.fontWeight400]}>{o}</Text>
          {o === "white" ? (
            <WhiteCard />
          ) : o === "black" ? (
            <BlackCard />
          ) : (
            <></>
          )}
        </CardChoice>
      ))}
    </View>
  );
};

export default CardPicker;
