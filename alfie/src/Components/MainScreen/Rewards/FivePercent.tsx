import React from "react";
import { Path, Svg } from "react-native-svg";

interface Props {
  width?: number;
  height?: number;
}

export default function FivePercent({
  width = 36,
  height = 36,
}: Props): JSX.Element {
  return (
    <Svg width={width} height={height} viewBox={`0 0 36 36`} fill="none">
      <Path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M18 36C27.9411 36 36 27.9411 36 18C36 8.05887 27.9411 0 18 0C8.05887 0 0 8.05887 0 18C0 27.9411 8.05887 36 18 36ZM21.3867 20.2246C21.3867 23.2324 19.082 25.2539 15.7129 25.2539C12.4414 25.2539 10.3516 23.4082 10.1953 21.0254L10.1855 20.8594H13.4375L13.457 20.9277C13.6816 21.8066 14.5312 22.5 15.7129 22.5C17.0605 22.5 18.0078 21.5918 18.0078 20.293V20.2734C18.0078 18.9844 17.0508 18.0957 15.7129 18.0957C15.1172 18.0957 14.5996 18.2617 14.1992 18.5547C13.9062 18.7598 13.6719 19.0234 13.5254 19.3066H10.4199L11.0254 10.9082H20.5078V13.6621H13.8477L13.5645 17.2266H13.6328C14.1895 16.2793 15.3418 15.6641 16.8066 15.6641C19.4629 15.6641 21.3867 17.5684 21.3867 20.2051V20.2246ZM25.6885 19.7363C25.6885 20.874 25.0684 21.582 24.0576 21.582C23.042 21.582 22.4219 20.874 22.4219 19.7363V19.7314C22.4219 18.5986 23.042 17.8906 24.0576 17.8906C25.0684 17.8906 25.6885 18.5986 25.6885 19.7314V19.7363ZM29.2334 17.9541L24.7852 25H23.6963L28.1494 17.9541H29.2334ZM23.4424 19.7363C23.4424 20.4053 23.6621 20.7715 24.0576 20.7715C24.4482 20.7715 24.6631 20.4053 24.6631 19.7363V19.7314C24.6631 19.0674 24.4482 18.7012 24.0576 18.7012C23.6621 18.7012 23.4424 19.0674 23.4424 19.7314V19.7363ZM30.5078 23.2178C30.5078 24.3555 29.8877 25.0635 28.877 25.0635C27.8613 25.0635 27.2412 24.3555 27.2412 23.2178V23.2129C27.2412 22.0801 27.8613 21.3721 28.877 21.3721C29.8877 21.3721 30.5078 22.0801 30.5078 23.2129V23.2178ZM28.2617 23.2178C28.2617 23.8867 28.4814 24.2529 28.877 24.2529C29.2676 24.2529 29.4824 23.8867 29.4824 23.2178V23.2129C29.4824 22.5488 29.2676 22.1826 28.877 22.1826C28.4814 22.1826 28.2617 22.5488 28.2617 23.2129V23.2178Z"
        fill="white"
      />
    </Svg>
  );
}
