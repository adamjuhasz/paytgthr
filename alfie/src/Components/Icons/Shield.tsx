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
          <Path d="M79.8,7.6 L52.3,1.5 C46.2,0.1 39.8,0.1 33.7,1.5 L6.2,7.6 C3,8.3 0.7,11.2 0.7,14.5 L0.7,34 C0.7,54.5 10.5,73.9 27,86 L37.4,93.6 C40.8,96.1 45.4,96.1 48.7,93.6 L59.1,86 C75.7,73.8 85.4,54.5 85.4,34 L85.4,14.5 C85.4,11.2 83.1,8.3 79.8,7.6 Z M66,37.1 L40.2,62.9 C39.6,63.5 38.7,63.5 38.2,62.9 L22.1,46.8 C21.5,46.2 21.5,45.3 22.1,44.8 L29.9,37 C30.5,36.4 31.4,36.4 31.9,37 L39.1,44.2 L56.1,27.2 C56.7,26.6 57.6,26.6 58.1,27.2 L66,35 C66.6,35.6 66.6,36.5 66,37.1 Z" />
        </G>
      </G>
    </Svg>
  );
};

export default Icon;
