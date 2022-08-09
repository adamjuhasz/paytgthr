import React, { useContext, useMemo } from "react";
import * as Clipboard from "expo-clipboard";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import Purchases from "./Purchases";
import useGetTransaction from "../../Hooks/UseGetTransactions";
import useGetCards from "../../Hooks/UseGetCards";
import useGetUser from "../../Hooks/UseGetUser";
import useGetLevel from "../../Hooks/UseGetLevel";
import { EnhancedCardModel } from "../../../Actions/Card/Types";
import toastContext from "../../Toast/ToastProvider";
import { useLockCard, useUnlockCard } from "../../Hooks/UseChangeCard";
import { PurchaseProps } from "./Types";

interface Props {
  gotoActivateCard: (cardId: string) => void;
  gotoSetCardPin: (cardId: string) => void;
}

export default function PurchasesHOC(props: Props): JSX.Element {
  const { showToast } = useContext(toastContext);

  const transactionData = useGetTransaction();
  const cardsData = useGetCards();
  const levels = useGetLevel();
  const userData = useGetUser();
  const [lockACard, lockStatus] = useLockCard();
  const [unlockACard, unlockStatus] = useUnlockCard();

  const copyToClipboard = useMemo(() => {
    return (notification: string, copiable: string, event: string) => {
      void Analytics.track(`User VirtualCard Copied ${event}`);
      showToast(notification);
      Clipboard.setString(copiable);
    };
  }, [showToast]);

  const purchases = useMemo(() => {
    let purchases: PurchaseProps[] = [];
    if (transactionData.data !== undefined) {
      purchases = transactionData.data;
    }
    return purchases;
  }, [transactionData.data]);

  const cardLocked = useMemo(() => {
    let cardLocked = true;
    if (userData === null) {
      return cardLocked;
    }

    if (
      userData.group.status === "groupactive" &&
      userData.user.ach.verified === true &&
      userData.partner?.verified === true
    ) {
      cardLocked = false;
    }
    return cardLocked;
  }, [userData]);

  const cardsToShow = useMemo(() => {
    let cards: EnhancedCardModel[] = [];
    if (cardsData.data !== undefined) {
      cards = cardsData.data.filter((c) => c.cardStatus !== "CLOSED");
    }
    const cardsToShow =
      lockStatus.isLoading || unlockStatus.isLoading ? [] : cards;
    return cardsToShow;
  }, [cardsData.data, lockStatus.isLoading, unlockStatus.isLoading]);

  return (
    <Purchases
      demoMode={false}
      purchases={purchases}
      cards={cardsToShow}
      copyToClipboard={copyToClipboard}
      locked={cardLocked}
      activateCard={props.gotoActivateCard}
      canSpend={levels.data.canSpend}
      maxSpend={levels.data.maxSpend}
      setPin={(cid: string) => props.gotoSetCardPin(cid)}
      lockCard={lockACard}
      unlockCard={unlockACard}
    />
  );
}
