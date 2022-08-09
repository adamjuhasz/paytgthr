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
          d="M80,10 C91.045695,10 100,18.954305 100,30 L100,30 L100,70 C100,81.045695 91.045695,90 80,90 L80,90 L20,90 C8.954305,90 0,81.045695 0,70 L0,70 L0,30 C0,18.954305 8.954305,10 20,10 L20,10 Z M80,20 L20,20 C14.4771525,20 10,24.4771525 10,30 L10,30 L10,70 C10,75.5228475 14.4771525,80 20,80 L20,80 L80,80 C85.5228475,80 90,75.5228475 90,70 L90,70 L90,30 C90,24.4771525 85.5228475,20 80,20 L80,20 Z M55,45 C57.7614237,45 60,47.2385763 60,50 C60,52.7614237 57.7614237,55 55,55 L45,55 C42.2385763,55 40,52.7614237 40,50 C40,47.2385763 42.2385763,45 45,45 L55,45 Z M32.5,30 C33.8807119,30 35,31.1192881 35,32.5 L35,52.5 C35,53.8807119 33.8807119,55 32.5,55 L17.5,55 C16.1192881,55 15,53.8807119 15,52.5 L15,32.5 C15,31.1192881 16.1192881,30 17.5,30 L32.5,30 Z M75,30 C77.7614237,30 80,32.2385763 80,35 C80,37.7614237 77.7614237,40 75,40 L45,40 C42.2385763,40 40,37.7614237 40,35 C40,32.2385763 42.2385763,30 45,30 L75,30 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;
