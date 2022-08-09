import React from "react";
import { Path, Svg } from "react-native-svg";

export default function Checkmark(_props: unknown): JSX.Element {
  return (
    <Svg width="16" height="12" viewBox="0 0 16 12" fill="none">
      <Path
        d="M2 6L6 10L14 2"
        stroke="white"
        strokeWidth="3"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </Svg>
  );
}
