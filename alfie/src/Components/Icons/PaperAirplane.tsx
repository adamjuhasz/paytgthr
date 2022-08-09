import React from "react";
import { G, NumberProp, Path, Svg } from "react-native-svg";

interface Props {
  color: string;
  size: NumberProp;
}

export const Icon = (props: Props): JSX.Element => {
  return (
    <Svg
      viewBox={`0 0 ${70} ${100}`}
      fill="none"
      width={props.size}
      height={props.size}
      color={props.color}
      preserveAspectRatio="xMinYMid"
    >
      <G stroke="none" strokeWidth="1" fill="none" fillRule="evenodd">
        <Path
          d="M93.4023196,0.176964757 C97.4916776,-0.910872406 101.290199,3.20714013 99.5795579,7.3126777 L99.5795579,7.3126777 L62.311261,96.7565903 C60.4953937,101.114672 54.3060164,101.070464 52.5525864,96.686889 L52.5525864,96.686889 L38.484044,61.5147999 L3.31311096,47.4474136 C-0.887815156,45.7670432 -1.10345411,40.0126864 2.72157356,37.9378639 L2.72157356,37.9378639 L3.2434097,37.688739 L92.4818862,0.504818052 C92.7227431,0.39258553 92.9703347,0.299551728 93.2223569,0.225716647 Z M79.611,27.834 L48.721,58.724 L57.5467779,80.7862362 L79.611,27.834 Z M72.143,20.395 L19.205154,42.452066 L41.264,51.274 L72.143,20.395 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;
