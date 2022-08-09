import React from "react";
import { Path, Rect, Svg } from "react-native-svg";

interface Props {
  width?: number;
  height?: number;
  color?: string;
}

export default function FivePercent({
  width = 32,
  height = 32,
  color = "white",
}: Props): JSX.Element {
  return (
    <Svg width={width} height={height} viewBox={`0 0 32 32`} fill="none">
      <Path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M16 11C15.4477 11 15 11.4477 15 12V15L12 15C11.4477 15 11 15.4477 11 16C11 16.5523 11.4477 17 12 17H15V20C15 20.5523 15.4477 21 16 21C16.5523 21 17 20.5523 17 20V17H20C20.5523 17 21 16.5523 21 16C21 15.4477 20.5523 15 20 15L17 15V12C17 11.4477 16.5523 11 16 11Z"
        fill={color}
      />
      <Rect
        x="0.5"
        y="0.5"
        width="31"
        height="31"
        rx="15.5"
        stroke={color}
        strokeDasharray="2 2"
      />
    </Svg>
  );
}
