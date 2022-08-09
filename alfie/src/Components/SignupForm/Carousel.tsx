import React, { useRef, useState } from "react";
import { FlatList, StyleSheet, View } from "react-native";
import { useLayout } from "@react-native-community/hooks";

import Banner, { colors } from "../Banner/Banner";
import useInterval from "../Hooks/UseInterval";

interface Props {
  texts: React.ReactNode[];
  trackTap?: () => void;
}

const Carousel = (props: Props): JSX.Element => {
  const { onLayout, width } = useLayout();
  const { onLayout: flOnLayout, width: listWidth } = useLayout();
  const listRef = useRef<FlatList>(null);
  const [currIndex, setIndex] = useState<number | null>(0);

  useInterval(() => {
    if (listRef !== null && currIndex !== null) {
      const nextIndex = (currIndex + 1) % props.texts.length;
      listRef.current?.scrollToIndex({
        animated: true,
        index: nextIndex,
        viewOffset: listWidth * 0.1,
      });
      setIndex(nextIndex);
    }
  }, 3000);

  return (
    <View style={[styles.container]} onLayout={onLayout}>
      <FlatList
        ref={listRef}
        style={[{ width: width + 2 * 10 }, styles.list]}
        pagingEnabled
        snapToAlignment="start"
        snapToInterval={listWidth * 0.85}
        decelerationRate="fast"
        data={listWidth <= 20 ? [] : props.texts}
        renderItem={(t) => (
          <Banner
            color={colors[t.index % colors.length]}
            text={t.item}
            width={listWidth * 0.8}
          />
        )}
        ItemSeparatorComponent={() => (
          <View style={[styles.seperator, { width: listWidth * 0.05 }]} />
        )}
        ListHeaderComponent={() => (
          <View style={[styles.seperator, { width: listWidth * 0.1 }]} />
        )}
        ListFooterComponent={() => (
          <View style={[styles.seperator, { width: listWidth * 0.1 }]} />
        )}
        horizontal
        onLayout={flOnLayout}
        showsHorizontalScrollIndicator={false}
        initialNumToRender={2}
        onScrollBeginDrag={() => {
          if (props.trackTap !== undefined) {
            props.trackTap();
          }

          setIndex(null);
        }}
      />
    </View>
  );
};

export default Carousel;

const styles = StyleSheet.create({
  container: {
    marginLeft: -10,
    marginTop: 40,
    minHeight: 1,
    overflow: "visible",
    position: "relative",
    width: "100%",
  },
  list: { overflow: "visible", position: "absolute", top: 0 },
  seperator: { height: 1 },
});
