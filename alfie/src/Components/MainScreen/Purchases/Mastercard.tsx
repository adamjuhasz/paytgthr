import React from "react";
import { StyleProp, ViewStyle } from "react-native";
import { Path, Svg } from "react-native-svg";

interface Props {
  style?: StyleProp<ViewStyle>;
  leftColor?: string;
  middlecolor?: string;
  rightColor?: string;
}

export default function Mastercard({
  leftColor = "#F26724",
  middlecolor = "#E31E27",
  rightColor = "#F9A01F",
  ...props
}: Props): JSX.Element {
  return (
    <Svg
      width="63"
      height="39"
      viewBox="0 0 63 39"
      fill="none"
      style={props.style}
    >
      <Path
        d="M39.8353 4.58179H23.2207V34.302H39.8353V4.58179Z"
        fill={middlecolor}
      />
      <Path
        d="M24.284 19.4413C24.284 13.4708 27.2082 8.03088 31.4615 4.58121C28.2715 2.06029 24.1511 0.46814 19.7649 0.46814C9.26447 0.46814 0.757812 8.95965 0.757812 19.4413C0.757812 29.923 9.26447 38.4146 19.7649 38.4146C24.1511 38.4146 28.2715 36.8224 31.4615 34.3015C27.0753 30.8518 24.284 25.4119 24.284 19.4413Z"
        fill={leftColor}
      />
      <Path
        d="M62.2985 19.4413C62.2985 29.923 53.7918 38.4146 43.2914 38.4146C38.9052 38.4146 34.7847 36.8224 31.5947 34.3015C35.981 30.8518 38.7722 25.4119 38.7722 19.4413C38.7722 13.4708 35.8481 8.03088 31.5947 4.58121C34.7847 2.06029 38.9052 0.46814 43.2914 0.46814C53.6589 0.46814 62.2985 8.95965 62.2985 19.4413Z"
        fill={rightColor}
      />
    </Svg>
  );
}
