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
          d="M7.14285714,4 C10.8059703,4 13.8250511,6.76167213 14.2376591,10.3195566 L14.2857143,11.1538462 L14.2857143,89.8461538 C14.2857143,93.797114 0.412607914,93.4040383 0,89.8461538 L0,11.1538462 C0,7.20288602 3.19796607,4 7.14285714,4 Z M35.7142857,32.6153846 C39.3773989,32.6153846 42.3964797,35.3770567 42.8090876,38.9349412 L42.8571429,39.7692308 L42.8571429,89.8461538 C42.8571429,93.797114 28.9840365,93.4040383 28.5714286,89.8461538 L28.5714286,39.7692308 C28.5714286,35.8182706 31.7693946,32.6153846 35.7142857,32.6153846 Z M64.2857143,11.1538462 C67.9488274,11.1538462 70.9679083,13.9155183 71.3805162,17.4734027 L71.4285714,18.3076923 L71.4285714,89.8461538 C71.4285714,93.797114 57.5554651,93.4040383 57.1428571,89.8461538 L57.1428571,18.3076923 C57.1428571,14.3567322 60.3408232,11.1538462 64.2857143,11.1538462 Z M92.8571429,43.3461538 C96.520256,43.3461538 99.5393369,46.107826 99.9519448,49.6657104 L100,50.5 L100,89.8461538 C100,93.797114 86.1268936,93.4040383 85.7142857,89.8461538 L85.7142857,50.5 C85.7142857,46.5490399 88.9122518,43.3461538 92.8571429,43.3461538 Z"
          fill={props.color}
          fillRule="nonzero"
          transform="translate(50.000000, 48.331949) scale(-1, 1) translate(-50.000000, -48.331949) "
        />
      </G>
    </Svg>
  );
};

export default Icon;
