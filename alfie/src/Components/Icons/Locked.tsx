import React from "react";
import { G, NumberProp, Path, Svg } from "react-native-svg";

interface Props {
  color: string;
  size: NumberProp;
}

export const LockedIcon = (props: Props): JSX.Element => {
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
          d="M50,0 C63.8071187,0 75,11.1928813 75,25 L75,25 L75,35 C80.5228475,35 85,39.4771525 85,45 L85,45 L85,90 C85,95.5228475 80.5228475,100 75,100 L75,100 L25,100 C19.4771525,100 15,95.5228475 15,90 L15,90 L15,45 C15,39.4771525 19.4771525,35 25,35 L25,35 L25,25 C25,11.1928813 36.1928813,0 50,0 Z M75,45 L25,45 L25,90 L75,90 L75,45 Z M50,54 C54.418278,54 58,57.581722 58,62 C58,65.1059972 56.2299403,67.7985748 53.6435229,69.1240308 L54.2481085,75.82011 C54.347459,76.9202024 53.5361974,77.892543 52.436105,77.9918935 C52.3762938,77.9972951 52.3162697,78 52.2562151,78 L47.7437849,78 C46.6392154,78 45.7437849,77.1045695 45.7437849,76 C45.7437849,75.9399454 45.7464899,75.8799212 45.7518915,75.82011 L46.3564771,69.1240308 C43.7700597,67.7985748 42,65.1059972 42,62 C42,57.581722 45.581722,54 50,54 Z M50,10 C41.7157288,10 35,16.7157288 35,25 L35,25 L35,35 L65,35 L65,25 C65,17.0115956 58.7554002,10.4816956 50.8813639,10.0254635 L50.8813639,10.0254635 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default LockedIcon;
