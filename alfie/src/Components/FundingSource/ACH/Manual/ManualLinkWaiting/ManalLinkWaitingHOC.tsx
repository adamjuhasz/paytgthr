import React, { useEffect } from "react";

import { registerForPushNotificationsAsync } from "../../../../../PlatformSpecific/PushNotifcations";
import ManualLinkWaiting from "./ManalLinkWaiting";

interface PropsHOC {
  gotoNext: () => void;
}

const ManualLinkWaitingHOC = (props: PropsHOC): JSX.Element => {
  useEffect(() => {
    void registerForPushNotificationsAsync();
  }, []);

  return <ManualLinkWaiting nextScreen={props.gotoNext} />;
};

export default ManualLinkWaitingHOC;
