import React, { useContext } from "react";
import { Alert } from "react-native";

import Purchases from "./Purchases";
import toastContext from "../../Toast/ToastProvider";
import { ghostPurchases } from "./GhostPurhcases";

export default function PurchasesHOC(_props: unknown): JSX.Element {
  const toast = useContext(toastContext);

  const copyToClipboard = (notification: string) => {
    toast.showToast(notification);
  };

  const alertNeedToBeLoggedIn = () => {
    Alert.alert("You need to be logged in to do this");
  };

  return (
    <Purchases
      demoMode={true}
      purchases={ghostPurchases}
      cards={[
        {
          cardId: "1",
          cardPlatform: {
            tag: "PayWithPrivacy",
            contents: "",
          },
          cardRevision: 1,
          cardDesign: "PhysicalBlack",
          cardholder: "",
          cardStatus: "ACTIVATED",
          cardMemo: null,
          createdAt: "",
          activatedAt: null,
          closedAt: null,
          updatedAt: "",
          cardLastFour: "",
          pan: "5252525252525252",
          expMonth: "12",
          expShortYear: "25",
          cvv: "525",
        },
        {
          cardId: "2",
          cardPlatform: {
            tag: "PayWithPrivacy",
            contents: "",
          },
          cardRevision: 1,
          cardDesign: "DigitalWallet",
          cardholder: "",
          cardStatus: "ACTIVATED",
          cardMemo: null,
          createdAt: "",
          activatedAt: null,
          closedAt: null,
          updatedAt: "",
          cardLastFour: "",
          pan: "5151515151515151",
          expMonth: "12",
          expShortYear: "27",
          cvv: "515",
        },
      ]}
      copyToClipboard={copyToClipboard}
      locked={false}
      activateCard={alertNeedToBeLoggedIn}
      canSpend={1900}
      maxSpend={2500}
      setPin={alertNeedToBeLoggedIn}
      lockCard={alertNeedToBeLoggedIn}
      unlockCard={alertNeedToBeLoggedIn}
    />
  );
}
