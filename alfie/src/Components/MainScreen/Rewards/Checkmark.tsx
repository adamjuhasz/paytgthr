import React from "react";
import { Defs, LinearGradient, Path, Rect, Stop, Svg } from "react-native-svg";

interface Props {
  width?: number;
  height?: number;
}

export default function Checkmark({
  width = 32,
  height = 32,
}: Props): JSX.Element {
  return (
    <Svg width={width} height={height} viewBox={`0 0 32 32`} fill="none">
      <Rect width="32" height="32" rx="16" fill="url(#paint0_linear)" />
      <Path
        d="M12 16L14.6667 19L20 13"
        stroke="white"
        strokeWidth="2"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
      <Defs>
        <LinearGradient
          id="paint0_linear"
          x1="0"
          y1="0"
          x2="19.5638"
          y2="38.345"
          gradientUnits="userSpaceOnUse"
        >
          <Stop offset="0.25" stopColor="#3AD79E" />
          <Stop offset="1" stopColor="#36B2B9" />
        </LinearGradient>
      </Defs>
    </Svg>
  );
}
