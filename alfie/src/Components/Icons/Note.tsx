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
          d="M80,0 C91.045695,0 100,8.954305 100,20 L100,20 L100,80 C100,91.045695 91.045695,100 80,100 L80,100 L20,100 C8.954305,100 0,91.045695 0,80 L0,80 L0,20 C0,8.954305 8.954305,0 20,0 L20,0 Z M80,10 L20,10 C14.4771525,10 10,14.4771525 10,20 L10,20 L10,80 C10,85.5228475 14.4771525,90 20,90 L20,90 L80,90 C85.5228475,90 90,85.5228475 90,80 L90,80 L90,20 C90,14.4771525 85.5228475,10 80,10 L80,10 Z M74,65 C76.7614237,65 79,67.2385763 79,70 C79,72.7614237 76.7614237,75 74,75 L25,75 C22.2385763,75 20,72.7614237 20,70 C20,67.2385763 22.2385763,65 25,65 L74,65 Z M65,45 C67.7614237,45 70,47.2385763 70,50 C70,52.7614237 67.7614237,55 65,55 L25,55 C22.2385763,55 20,52.7614237 20,50 C20,47.2385763 22.2385763,45 25,45 L65,45 Z M71,25 C73.7614237,25 76,27.2385763 76,30 C76,32.7614237 73.7614237,35 71,35 L25,35 C22.2385763,35 20,32.7614237 20,30 C20,27.2385763 22.2385763,25 25,25 L71,25 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;
