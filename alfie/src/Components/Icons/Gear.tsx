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
          d="M52.2727272,2.20268248e-13 C54.7657315,2.20268248e-13 57.1305857,2.03518769 57.5547768,4.54571894 L58.4719281,9.96980886 C61.3130939,10.5681439 64.0456449,11.4613286 66.6360841,12.6158642 L70.8925634,7.95233358 C72.4890813,6.20330083 75.3624953,5.82058943 77.3105347,7.09749079 C79.2585312,8.37445803 80.0808816,11.1797592 79.1472842,13.3633392 L76.7256997,19.0277959 C79.6124618,21.5210445 82.1470027,24.4110699 84.2445949,27.6131395 L89.9952255,26.0203679 C92.2673751,25.3910723 94.9050883,26.6034873 95.8867479,28.7283609 C96.8683573,30.853258 96.0929934,33.6721588 94.154899,35.0245638 L89.2537833,38.4450709 C90.3310879,42.1102085 90.9090887,45.9890548 90.9090887,50.0029084 L90.908,49.867 L96.4776883,51.6562522 C98.7228245,52.3775998 100.28081,54.8445053 99.9575499,57.1662404 C99.6342827,59.4879744 97.4630039,61.4258455 95.1078595,61.4945961 L89.2225403,61.6665075 C88.112882,65.4041539 86.4835885,68.918262 84.4184026,72.125084 L88.1895177,76.7699848 C89.6832274,78.6092264 89.6587874,81.5346535 88.1349644,83.3041393 C86.6110716,85.0735645 83.7444603,85.5051346 81.732174,84.2680478 L76.5814328,81.1020406 C73.72495,83.5461644 70.5277401,85.60355 67.0709033,87.1930925 L67.7515675,93.3764204 C68.011892,95.7401084 66.4129123,98.1801421 64.18015,98.8264007 C61.9473805,99.4726341 59.3103742,98.2586533 58.2902281,96.1148861 L55.6314899,90.5299206 C53.7905767,90.7833964 51.9105837,90.9143789 50,90.9143789 C48.0894163,90.9143789 46.2094233,90.7833964 44.3685101,90.5299206 L41.7097719,96.1148861 C40.6896258,98.2586533 38.0526195,99.4726341 35.81985,98.8264007 C33.5870877,98.1801421 31.988108,95.7401084 32.2484325,93.3764204 L32.9290967,87.1930925 C29.4722599,85.60355 26.27505,83.5461644 23.4185672,81.1020406 L18.267826,84.2680478 C16.2555397,85.5051346 13.3889284,85.0735645 11.8650356,83.3041393 C10.3412126,81.5346535 10.3167726,78.6092264 11.8104823,76.7699848 L15.5815974,72.125084 C13.5164115,68.918262 11.887118,65.4041539 10.7774597,61.6665075 L4.89214051,61.4945961 C2.53699607,61.4258455 0.365717309,59.4879744 0.0424501083,57.1662404 C-0.280809968,54.8445053 1.27717555,52.3775998 3.52231172,51.6562522 L9.0911303,49.8675915 C9.10396985,45.9018718 9.68102288,42.0690059 10.7462167,38.4450709 L5.84510097,35.0245638 C3.90700662,33.6721588 3.13164272,30.853258 4.11325207,28.7283609 C5.09491171,26.6034873 7.73262489,25.3910723 10.0047745,26.0203679 L15.7554051,27.6131395 C17.8529973,24.4110699 20.3875382,21.5210445 23.2743003,19.0277959 L20.8527158,13.3633392 C19.9191184,11.1797592 20.7414688,8.37445803 22.6894653,7.09749079 C24.6375047,5.82058943 27.5109187,6.20330083 29.1074366,7.95233358 L33.3639159,12.6158642 C37.3449066,10.8415722 41.661531,9.68452796 46.1922103,9.26631718 L46.9906775,4.54571894 C47.4148686,2.03518769 49.7797228,2.20268248e-13 52.2727272,2.20268248e-13 Z M66.6790978,57.2467722 C65.2650042,60.511177 62.8805169,63.3951226 59.6354562,65.4228616 C56.3905809,67.4504846 52.7534477,68.3294246 49.1997159,68.1695105 L45.1381696,81.4538175 C46.7232765,81.69687 48.3469164,81.822941 50,81.822941 C64.2940489,81.822941 76.3866209,72.3967495 80.4014077,59.4210171 Z M29.4566747,25.7031105 C22.5602521,31.5399793 18.1818199,40.2600953 18.1818199,50.0029084 C18.1818199,62.727932 25.6508992,73.7083571 36.4439224,78.7988068 L40.5052723,65.5135918 C38.1730719,64.0882958 36.1278967,62.1134446 34.5809445,59.6378036 C30.0910909,52.4525358 31.2326941,43.3443724 36.8137784,37.4770129 Z M57.7095277,45.1854608 C55.04877,40.9273583 49.4401265,39.632329 45.1822719,42.2929318 C40.9244173,44.9535346 39.6297145,50.5622535 42.2904723,54.820356 C44.95123,59.0784585 50.5598735,60.3734878 54.8177281,57.712885 C59.0755827,55.0522821 60.3702855,49.4435633 57.7095277,45.1854608 Z M50,18.1828758 C45.4318716,18.1828758 41.0885865,19.145601 37.1619472,20.879238 L44.5233507,32.6598741 C52.243212,30.2163352 60.9294135,33.1830841 65.4190555,40.3680132 C66.9660076,42.8436542 67.8444463,45.5475632 68.1031246,48.2685397 L81.814,50.44 L81.8181801,50.0029084 C81.8181801,32.6049268 67.8561808,18.4681348 50.5261718,18.1871388 Z"
          fill={props.color}
          fillRule="nonzero"
        />
      </G>
    </Svg>
  );
};

export default Icon;