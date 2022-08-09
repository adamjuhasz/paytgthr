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
          d="M80,10 C91.045695,10 100,18.954305 100,30 L100,30 L100,70 C100,81.045695 91.045695,90 80,90 L80,90 L20,90 C8.954305,90 0,81.045695 0,70 L0,70 L0,30 C0,18.954305 8.954305,10 20,10 L20,10 Z M90,40 L10,40 L10,70 C10,75.5228475 14.4771525,80 20,80 L20,80 L80,80 C85.5228475,80 90,75.5228475 90,70 L90,70 L90,40 L90,40 Z M27.5,50 C28.8807119,50 30,51.1192881 30,52.5 L30,57.5 C30,58.8807119 28.8807119,60 27.5,60 L17.5,60 C16.1192881,60 15,58.8807119 15,57.5 L15,52.5 C15,51.1192881 16.1192881,50 17.5,50 L27.5,50 Z M80,20 L20,20 C14.4771525,20 10,24.4771525 10,30 L10,30 L90,30 C90,24.4771525 85.5228475,20 80,20 L80,20 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;
