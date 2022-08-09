// https://thenounproject.com/urikelman/uploads/?i=3988323

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
          d="M50,0 C77.6142375,0 100,22.3857625 100,50 C100,77.6142375 77.6142375,100 50,100 C22.3857625,100 0,77.6142375 0,50 C0,22.3857625 22.3857625,0 50,0 Z M50,10 C27.90861,10 10,27.90861 10,50 C10,72.09139 27.90861,90 50,90 C72.09139,90 90,72.09139 90,50 C90,27.90861 72.09139,10 50,10 Z M70,45 C72.7614237,45 75,47.2385763 75,50 C75,52.5641792 73.069799,54.6775358 70.5831056,54.9663613 L70,55 L30,55 C27.2385763,55 25,52.7614237 25,50 C25,47.4358208 26.930201,45.3224642 29.4168944,45.0336387 L30,45 L70,45 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;
