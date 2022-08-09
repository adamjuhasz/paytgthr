import React, { useContext } from "react";
import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";
import { ActivityIndicator, StyleSheet, View } from "react-native";

import { State } from "../../../State/State";
import {
  getRecentPayments,
  queryPath,
} from "../../../Actions/Payments/GetRecent";
import {
  getTransactions,
  queryPath as trxQueryPath,
} from "../../../Actions/User/GetTransactions";
import {
  getMostRecentLedger,
  queryPath as ledgerQueryPath,
} from "../../../Actions/Ledger/GetLedger";

import Payments, { Credit, UpcomingPaymentProps } from "./Payments";
import { ThemeContext } from "../../Theming/ThemeContext";

const staleTimeMs = 30 * 1000; // 30 sec

const PaymentsHOC = (): JSX.Element => {
  const theme = useContext(ThemeContext);
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const paymentsQuery = useQuery([queryPath], getRecentPayments(baseURL), {
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });
  const transactionQuery = useQuery([trxQueryPath], getTransactions(baseURL), {
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });
  const ledgerQuery = useQuery(
    [ledgerQueryPath],
    getMostRecentLedger(baseURL),
    {
      staleTime: staleTimeMs,
      refetchOnMount: false,
      refetchOnWindowFocus: false,
    }
  );

  if (
    paymentsQuery.status === QueryStatus.Success &&
    transactionQuery.status === QueryStatus.Success &&
    ledgerQuery.status === QueryStatus.Success
  ) {
    if (
      paymentsQuery.data !== undefined &&
      transactionQuery.data !== undefined &&
      ledgerQuery.data !== undefined
    ) {
      const pendingPurchases = transactionQuery.data
        .filter((trx) => trx.isPending)
        .reduce((accum, trx) => accum + parseFloat(trx.amount), 0);

      const pendingPayments = paymentsQuery.data
        .filter((payment) => payment.status.tag === "PaymentPending")
        .map((payment) =>
          payment.type === "DebitFromUser"
            ? payment.amount
            : -1 * payment.amount
        )
        .reduce((accum, curr) => accum + curr, 0);

      const balance = ledgerQuery.data.balance + pendingPayments;
      let upcomingPayments: UpcomingPaymentProps[] = [];
      let credits: Credit[] = [];
      if (balance > 0) {
        credits = [{ amount: balance }];
      } else if (balance < 0) {
        const today = new Date();
        let nextPayment = new Date();
        switch (today.getDay()) {
          case 0: // Sunday
          case 1: // Monday
          case 2: // Tueaday
          case 3: // Wednesday
          case 4: // Thursday
            nextPayment.setDate(nextPayment.getDate() + 1); // we will bill you tomorrow
            break;
          case 5: // Friday
            nextPayment.setDate(nextPayment.getDate() + 3); // on monday
            break;
          case 6: // Saturday
            nextPayment.setDate(nextPayment.getDate() + 2); // on monday
            break;
        }
        // we batch ACHs at 20 UTC
        if (today.getUTCHours() < 20) {
          nextPayment = today;
        }
        upcomingPayments = [
          {
            amount: balance,
            date: nextPayment,
          },
        ];
      }

      return (
        <Payments
          payments={paymentsQuery.data}
          pendingPurchases={pendingPurchases}
          upcomingPayments={upcomingPayments}
          credits={credits}
          demoMode={false}
        />
      );
    }
  }

  return (
    <View style={[StyleSheet.absoluteFill, styles.activity]}>
      <theme.background style={styles.fullScreenAbsolute} />
      <ActivityIndicator size="large" color={theme.textColor} />
    </View>
  );
};

export default PaymentsHOC;

const styles = StyleSheet.create({
  activity: { alignItems: "center", justifyContent: "center" },
  fullScreenAbsolute: {
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
});
