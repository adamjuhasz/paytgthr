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
          d="M50,0 C77.6142375,0 100,22.3857625 100,50 C100,77.6142375 77.6142375,100 50,100 C22.3857625,100 0,77.6142375 0,50 C0,22.3857625 22.3857625,0 50,0 Z M50,10 C27.90861,10 10,27.90861 10,50 C10,72.09139 27.90861,90 50,90 C72.09139,90 90,72.09139 90,50 C90,27.90861 72.09139,10 50,10 Z M50.5,40 C52.9852814,40 55,42.0147186 55,44.5 C55,44.5825046 54.9977797,44.6644906 54.9933955,44.7459015 C54.9978533,44.8294756 55,44.9144871 55,45 L55,70 C55,72.7614237 52.7614237,75 50,75 C47.2385763,75 45,72.7614237 45,70 L45,49 L43.5,49 C41.0147186,49 39,46.9852814 39,44.5 C39,42.0147186 41.0147186,40 43.5,40 L50.5,40 Z M50,25 C52.7614237,25 55,27.2385763 55,30 C55,32.7614237 52.7614237,35 50,35 C47.2385763,35 45,32.7614237 45,30 C45,27.2385763 47.2385763,25 50,25 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;
