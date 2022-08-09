import React, { useEffect } from "react";
import { Alert } from "react-native";
import console from "../../../../Global/Console";
import Analytics from "../../../../PlatformSpecific/SegmentAnalytcs";
import { defaultTo } from "lodash";
import { useMutation, useQuery } from "react-query";
import { useSelector } from "react-redux";

import { State } from "../../../../State/State";
import PlaidLink from "./PlaidLink";
import {
  chooseAccount,
  getLinkToken,
  queryPath,
} from "../../../../Actions/FundingSource/Plaid";

interface Props {
  goBack: () => void;
  goNext: () => void;
}

const PlaidHOC = (props: Props): JSX.Element => {
  useEffect(() => {
    void Analytics.track("PlaidHOC Rendered");
  });

  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const staleTimeMs = 20 * 50 * 1000; // 20 min

  const query = useQuery([queryPath], getLinkToken(baseURL), {
    // do as much as we can so that this does not re-render so that we don't re-render the webview
    staleTime: staleTimeMs,
    cacheTime: staleTimeMs,
    refetchOnWindowFocus: false,
    refetchOnMount: false,
    refetchOnReconnect: false,
    notifyOnStatusChange: false,
  });
  const [mutate, { isSuccess, isError }] = useMutation(
    chooseAccount(baseURL),
    {}
  );

  useEffect(() => {
    if (isSuccess) {
      props.goNext();
    }
  }, [isSuccess, props]);

  useEffect(() => {
    if (isError) {
      Alert.alert("We had an error loading your account, can you try again?");
      props.goBack();
    }
  }, [isError, props]);

  const submit = async (publicToken: string, accountId: string) => {
    console.log(publicToken, accountId);
    await mutate({ publicToken, accountId });
  };

  if (query.data === undefined) {
    return <></>;
  }

  return (
    <PlaidLink
      linkToken={query.data.token}
      onError={({ nativeEvent: { domain, description, code } }) => {
        void Analytics.track("PlaidLink error", { domain, description, code });
      }}
      onEvent={(evt) => {
        void Analytics.track(
          `PlaidLink ${defaultTo(evt.eventName, "Unknown")}`,
          evt.metadata
        );
      }}
      onExit={() => {
        props.goBack();
      }}
      onSuccess={(evt) => {
        if (evt.metadata.accounts.length === 0) {
          void Analytics.track(`PlaidLink Error NoAccountSelected`, {});
          Alert.alert(
            "Error accessing your account, please contact support at hi@paytgthr.com"
          );
          props.goBack();
          return;
        }

        const minimumBalance = 75;
        const lowBalanceMsg = `You need to link an account with a balance of at least $${minimumBalance}`;
        const account = evt.metadata.accounts[0];
        let currentBalance = 0;
        if (account.balance === undefined || account.balance === null) {
          currentBalance = 0;
        } else {
          if (
            account.balance.available !== undefined &&
            account.balance.available !== null
          ) {
            currentBalance = account.balance.available;
          } else if (
            account.balance.current !== undefined &&
            account.balance.current !== null
          ) {
            currentBalance = account.balance.current;
          }
        }
        console.log("PlaidLink chosen account", account);

        void Analytics.track(`PlaidLink Balances`, {
          minimumBalance,
          currentBalance: defaultTo(
            account.balance?.current,
            JSON.stringify(account.balance)
          ),
          availableBalance: defaultTo(
            account.balance?.available,
            JSON.stringify(account.balance)
          ),
          account,
          institution: evt.metadata.institution,
        });

        if (currentBalance < minimumBalance) {
          void Analytics.track(`PlaidLink Error BalanceTooLow`, {
            minimumBalance,
            currentBalance,
            account,
            institution: evt.metadata.institution,
          });
          Alert.alert(lowBalanceMsg);
          props.goBack();
          return;
        }

        let accountId: string;
        if (account.id !== undefined) {
          accountId = account.id;
        } else if (account._id !== undefined) {
          accountId = account._id;
        } else {
          accountId = "unknown account id";
        }

        void submit(evt.publicToken, accountId);
      }}
    />
  );
};

export default PlaidHOC;
