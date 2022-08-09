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
          d="M69.1586509,41.8162433 C67.1657649,40.66565 64.6174716,41.3484632 63.4668784,43.3413491 C62.3984704,45.1918861 62.9109077,47.5213202 64.5851801,48.7658843 L64.9919842,49.0331216 L79.4257409,57.366455 C85.4043987,60.8182347 87.4528382,68.4631147 84.0010585,74.4417725 C80.6725567,80.2069068 73.4451133,82.3175396 67.5724204,79.3659484 L66.9257409,79.0170901 L52.4919842,70.6837567 C46.7268499,67.3552549 44.6162171,60.1278115 47.5678084,54.2551186 L47.9166667,53.6084392 C49.0672599,51.6155533 48.3844467,49.0672599 46.3915608,47.9166667 C44.3986749,46.7660734 41.8503815,47.4488866 40.6997883,49.4417725 C35.098216,59.1439803 38.1875486,71.4789886 47.5523085,77.4321125 L48.3253175,77.9006351 L62.7590743,86.2339684 C72.7235039,91.9869346 85.4649707,88.5728688 91.2179369,78.6084392 C96.8195092,68.9062314 93.7301766,56.5712231 84.3654166,50.6180992 L83.5924076,50.1495766 L69.1586509,41.8162433 Z M37.2409257,13.7660316 C27.2764961,8.01306543 14.5350293,11.4271312 8.78206314,21.3915608 C3.18049085,31.0937686 6.26982342,43.4287769 15.6345834,49.3819008 L16.4075924,49.8504234 L30.8413491,58.1837567 C32.8342351,59.33435 35.3825284,58.6515368 36.5331216,56.6586509 C37.6015296,54.8081139 37.0890923,52.4786798 35.4148199,51.2341157 L35.0080158,50.9668784 L20.5742591,42.633545 C14.5956013,39.1817653 12.5471618,31.5368853 15.9989415,25.5582275 C19.3274433,19.7930932 26.5548867,17.6824604 32.4275796,20.6340516 L33.0742591,20.9829099 L47.5080158,29.3162433 C53.2731501,32.6447451 55.3837829,39.8721885 52.4321916,45.7448814 L52.0833333,46.3915608 C50.9327401,48.3844467 51.6155533,50.9327401 53.6084392,52.0833333 C55.6013251,53.2339266 58.1496185,52.5511134 59.3002117,50.5582275 C64.901784,40.8560197 61.8124514,28.5210114 52.4476915,22.5678875 L51.6746825,22.0993649 L37.2409257,13.7660316 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;