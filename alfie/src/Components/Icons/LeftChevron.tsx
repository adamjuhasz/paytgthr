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
      <G stroke="none" strokeWidth="1" fill="none" fill-rule="evenodd">
        <G fill={props.color} fillRule="nonzero">
          <Path d="M3.4,54.2 L42.1,92.9 C45.5,96.3 51.1,96.3 54.5,92.9 C57.9,89.5 57.9,83.9 54.5,80.5 L22.1,48 L54.6,15.5 C58,12.1 58,6.5 54.6,3.1 C52.9,1.4 50.6,0.5 48.4,0.5 C46.2,0.5 43.9,1.4 42.2,3.1 L3.4,41.8 C0,45.2 0,50.8 3.4,54.2 Z" />
        </G>
      </G>
    </Svg>
  );
};

export default Icon;
