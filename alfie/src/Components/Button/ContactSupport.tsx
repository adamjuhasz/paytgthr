import React from "react";
import {
  connectActionSheet,
  useActionSheet,
} from "@expo/react-native-action-sheet";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import Button, { ButtonStyle } from "./Button";
import { sendEmail } from "../../Helpers/SendEmail";

interface Props {
  emailSubject: string;
  style?: ButtonStyle;
}

const ContactSupportButton = (props: Props) => {
  const { showActionSheetWithOptions } = useActionSheet();
  const style = props.style === undefined ? "Primary" : props.style;

  const popUp = () => {
    void Analytics.track("ContactSupport Button Opened");
    showActionSheetWithOptions(
      {
        options: [
          "Close", // 0
          "Email Pay Tgthr", // 1
        ],
        cancelButtonIndex: 0,
      },
      (index) => {
        switch (index) {
          case 0:
            void Analytics.track("ContactSupport Button Closed");
            // close
            return;

          case 1:
            void Analytics.track("ContactSupport Email Created");
            void sendEmail(props.emailSubject);
            return;
        }
      }
    );
  };
  return <Button text="Contact support" style={style} onPress={popUp} />;
};

export default connectActionSheet(ContactSupportButton);
