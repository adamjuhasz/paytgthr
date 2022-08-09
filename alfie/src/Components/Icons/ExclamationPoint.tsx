import React from "react";
import { G, Path, Svg } from "react-native-svg";
import { IconProps } from "./Types";

export const Icon = (props: IconProps): JSX.Element => {
  return (
    <Svg
      viewBox={`3 0 ${26} ${90}`}
      fill="none"
      width={props.size}
      height={props.size}
      color={props.color}
      preserveAspectRatio="xMidYMid"
    >
      <G stroke="none" strokeWidth="1" fill="none" fillRule="evenodd">
        <G fill={props.color} fillRule="nonzero">
          <Path d="M22.4,80.6 C22.4,85.8 18.2,90 13,90 C7.8,90 3.6,85.8 3.6,80.6 C3.6,75.4 7.8,71.2 13,71.2 C18.2,71.2 22.4,75.5 22.4,80.6 Z" />
          <Path d="M26,9.5 L25.8,11.1 L20.7,53.5 C20.7,57.8 17.2,61.3 12.9,61.3 C8.6,61.3 5.1,57.8 5.1,53.5 L0,11.1 L0,9.5 C0,4.2 5.8,0 13,0 C20.2,0 26,4.2 26,9.5 Z" />
        </G>
      </G>
    </Svg>
  );
};

export default Icon;
