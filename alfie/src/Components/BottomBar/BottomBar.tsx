import React, { useContext, useState } from "react";
import { Pressable, StyleSheet, Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { ThemeContext } from "../Theming/ThemeContext";
import { IconProps } from "../Icons/Types";
import TextStyles from "../Styling/Text";
import { pinkColor, whiteColor } from "../Styling/Colors";

export interface Page {
  text: string;
  icon: (p: IconProps) => JSX.Element;
  selected: boolean;
  action: () => void;
}

interface Props {
  pages: Page[];
}

const bottomBarSize = 65;
export const iconSize = 30;

const IconButton = (page: Page) => {
  const [pressed, setPressed] = useState(false);
  let iconColor = "#AFB4BE";
  if (pressed) {
    iconColor = "#EA4352";
  }

  return (
    <Pressable
      disabled={page.selected}
      key={page.text}
      style={[]}
      onPressIn={() => {
        setPressed(true);
      }}
      onPressOut={() => {
        setPressed(false);
      }}
      onPress={page.action}
      hitSlop={10}
    >
      <View
        style={[
          iconStyles.icon,
          page.selected ? iconStyles.selectedIcon : undefined,
        ]}
      >
        <page.icon
          size={iconSize}
          color={page.selected ? whiteColor : iconColor}
        />
        {page.selected === false ? undefined : (
          <Text
            style={[TextStyles.fontWeight400, iconStyles.selectedText]}
            selectable={false}
          >
            {page.text}
          </Text>
        )}
      </View>
    </Pressable>
  );
};

const iconStyles = StyleSheet.create({
  icon: { alignItems: "center", flexDirection: "row" },
  selectedIcon: {
    backgroundColor: pinkColor,
    borderRadius: 30,
    paddingHorizontal: 20,
    paddingVertical: 10,
  },
  selectedText: {
    color: whiteColor,
    marginLeft: 5,
  },
});

const BottomBar = (props: React.PropsWithChildren<Props>): JSX.Element => {
  const insets = useSafeAreaInsets();
  const theme = useContext(ThemeContext);

  return (
    <View style={[StyleSheet.absoluteFill, styles.container]}>
      <View style={[styles.children]}>{props.children}</View>
      <View style={[styles.bar, { height: bottomBarSize }]}>
        <theme.background style={[StyleSheet.absoluteFill]} />
        {props.pages.map((page) => (
          <IconButton key={page.text} {...page} />
        ))}
      </View>
      <View style={{ height: insets.bottom }}>
        <theme.background />
      </View>
    </View>
  );
};

export default BottomBar;

const styles = StyleSheet.create({
  bar: {
    alignItems: "center",
    flexDirection: "row",
    justifyContent: "space-evenly",
    overflow: "visible",
  },
  children: { flex: 1 },
  container: {
    flexDirection: "column",
  },
});
