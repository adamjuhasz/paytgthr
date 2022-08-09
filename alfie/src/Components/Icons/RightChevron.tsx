import React from "react";
import { G, Path, Svg } from "react-native-svg";
import { IconProps } from "./Types";

export const Icon = (props: IconProps): JSX.Element => {
  return (
    <Svg
      viewBox={`0 0 ${58} ${96}`}
      fill="none"
      width={props.size}
      height={props.size}
      color={props.color}
      preserveAspectRatio="xMinYMid"
    >
      <G stroke="none" strokeWidth="1" fill="none" fillRule="evenodd">
        <G fill={props.color} fillRule="nonzero">
          <Path d="M54.6,41.8 L15.9,3.1 C12.5,-0.3 6.9,-0.3 3.5,3.1 C0.1,6.5 0.1,12.1 3.5,15.5 L35.9,48 L3.4,80.5 C0,83.9 0,89.5 3.4,92.9 C5.1,94.6 7.4,95.5 9.6,95.5 C11.9,95.5 14.1,94.6 15.8,92.9 L54.5,54.2 C58,50.8 58,45.2 54.6,41.8 Z" />
        </G>
      </G>
    </Svg>
  );
};

export default Icon;
