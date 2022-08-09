import React, { useEffect } from "react";
import { Platform } from "react-native";
import { useDispatch, useSelector } from "react-redux";
import * as Device from "expo-device";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import { requestReview } from "expo-store-review";
import { useLocation } from "../../PlatformSpecific/react-router";

import { State } from "../../State/State";
import { AlfieDispatch } from "../../State/Store";

import useGetTransaction from "../Hooks/UseGetTransactions";

const StoreReview = (): JSX.Element => {
  const { lastAsk } = useSelector((state: State) => ({
    lastAsk: state.askedForReview,
  }));
  const dispatch = useDispatch<AlfieDispatch>();
  const transactions = useGetTransaction();
  const location = useLocation();

  const askReview = async () => {
    const now = new Date();
    try {
      await requestReview();
      dispatch({ type: "askedForReview", time: now });
      await Analytics.track("StoreReview Requested", {
        lastTime: lastAsk,
        now: now,
        platform: Platform.OS,
        screen: location.pathname,
      });
    } catch (e) {
      await Analytics.track("StoreReview Requested Error", {
        lastTime: lastAsk,
        now: now,
      });
    }
  };

  useEffect(() => {
    const shouldAsk =
      transactions.data?.[0]?.isDeclined === false && Device.isDevice === true;

    if (shouldAsk) {
      const now = new Date();

      if (lastAsk === null) {
        void askReview();
      } else {
        const lastDate = new Date(lastAsk);
        const now_epoch = now.valueOf();
        const last_epoch = lastDate.valueOf();
        const daysDiff = (now_epoch - last_epoch) / 86400000;
        if (daysDiff >= 90) {
          void askReview();
        } else {
          return;
        }
      }
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [transactions.data, lastAsk]);

  return <></>;
};

export default StoreReview;
