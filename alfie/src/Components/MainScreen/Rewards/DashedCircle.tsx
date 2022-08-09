import React from "react";
import { Rect, Svg } from "react-native-svg";

interface Props {
  width?: number;
  height?: number;
}

export default function FivePercent({
  width = 32,
  height = 32,
}: Props): JSX.Element {
  return (
    <Svg width={width} height={height} viewBox={`0 0 32 32`} fill="none">
      <Rect
        x="0.5"
        y="0.5"
        width="31"
        height="31"
        rx="15.5"
        stroke="white"
        strokeDasharray="2 2"
      />
    </Svg>
  );
}
