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
          d="M72.5,5 C76.6421356,5 80,8.35786438 80,12.5 C80,16.6421356 76.6421356,20 72.5,20 C72.4059754,20 72.312355,19.9982698 72.2191688,19.9948395 L63.3186189,30.1328391 C66.5450834,32.6574901 69.2872019,36.0720366 71.3647423,40.0988363 L81.5383876,40.0986185 L92.870193,29.1524924 L93.3493459,28.753074 C95.0182389,27.5589564 97.3531688,27.7366967 98.8201694,29.2588697 C100.43387,30.93326 100.3863,33.6003564 98.7139198,35.2159961 L98.7139198,35.2159961 L86.1593618,47.3446008 L85.6589239,47.7584352 C84.9542845,48.2548238 84.1095003,48.525845 83.2374984,48.525845 L83.2374984,48.525845 L74.4934905,48.5251383 C74.9630809,50.4411236 75.3029008,52.4293465 75.5007225,54.4709702 L95.7920606,54.4710728 C98.1160414,54.4710728 100,56.3572954 100,58.6840689 C100,60.8318598 98.3947335,62.60427 96.3198956,62.8642397 L95.7920606,62.8970649 L75.5007071,62.8973259 C75.2239255,65.7537486 74.6691873,68.5056398 73.8699808,71.1014117 L83.2374984,71.1013172 C84.1095003,71.1013172 84.9542845,71.3723384 85.6589239,71.868727 L86.1593618,72.2825614 L98.7139198,84.4111661 C100.3863,86.0268058 100.43387,88.6939022 98.8201694,90.3682925 C97.3531688,91.8904655 95.0182389,92.0682058 93.3493459,90.8740882 L92.870193,90.4746698 L81.5383876,79.5280672 L70.0907068,79.5274335 C65.4301378,87.0989814 58.2431416,92 49.9776457,92 C41.7121498,92 34.5251536,87.0989814 29.8645846,79.5274335 L18.4601779,79.5280672 L7.12980697,90.4746698 L6.65065413,90.8740882 C4.98176108,92.0682058 2.64683115,91.8904655 1.17983062,90.3682925 C-0.433869971,88.6939022 -0.386300415,86.0268058 1.2860802,84.4111661 L1.2860802,84.4111661 L13.8406382,72.2825614 L14.3410761,71.868727 C15.0457155,71.3723384 15.8904997,71.1013172 16.7625016,71.1013172 L16.7625016,71.1013172 L26.0853106,71.1014117 C25.2861041,68.5056398 24.7313659,65.7537486 24.4545843,62.8973259 L4.20794358,62.8970649 C1.88396284,62.8970649 4.20872814e-06,61.0108423 4.20872814e-06,58.6840689 C4.20872814e-06,56.536278 1.60527073,54.7638677 3.68010861,54.503898 L4.20794358,54.4710728 L24.4545689,54.4709702 C24.6523905,52.4293465 24.9922105,50.4411236 25.4618009,48.5251383 L16.7625016,48.525845 C15.8904997,48.525845 15.0457155,48.2548238 14.3410761,47.7584352 L13.8406382,47.3446008 L1.2860802,35.2159961 C-0.386300415,33.6003564 -0.433869971,30.93326 1.17983062,29.2588697 C2.64683115,27.7366967 4.98176108,27.5589564 6.65065413,28.753074 L7.12980697,29.1524924 L18.4601779,40.0986185 L28.5905491,40.0988363 C30.668294,36.0716402 33.4107479,32.6568178 36.6376253,30.1320935 L27.6614647,19.9106101 C27.2829312,19.9694593 26.8950322,20 26.5,20 C22.3578644,20 19,16.6421356 19,12.5 C19,8.35786438 22.3578644,5 26.5,5 C30.6421356,5 34,8.35786438 34,12.5 C34,13.0696018 33.9365024,13.6243729 33.8161996,14.157621 L44.3473693,26.1513298 C46.152024,25.6388538 48.0350485,25.3681377 49.9776457,25.3681377 C51.9202429,25.3681377 53.8032674,25.6388538 55.6079221,26.1513298 L65.4190897,14.9781787 C65.1476027,14.2024059 65,13.3684402 65,12.5 C65,8.35786438 68.3578644,5 72.5,5 Z M49.9776457,33.7941299 C40.7387799,33.7941299 32.667712,44.7102536 32.667712,58.6840689 C32.667712,70.480209 38.4192084,80.097418 45.76951,82.8084226 L45.7697063,58.6840689 C45.7697063,56.3572954 47.6536649,54.4710728 49.9776457,54.4710728 C52.1228587,54.4710728 53.8931416,56.0782684 54.1527992,58.1555996 L54.1855851,58.6840689 L54.1853649,82.8085762 C61.5358657,80.0977812 67.2875794,70.4804317 67.2875794,58.6840689 C67.2875794,44.7102536 59.2165115,33.7941299 49.9776457,33.7941299 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;