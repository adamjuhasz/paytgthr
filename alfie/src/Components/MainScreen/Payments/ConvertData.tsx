import React from "react";

import { Payment } from "../../../Actions/Payments/Type";
import { TextData } from "../../SectionedList/Types";

import PaymentPill from "./PaymentPill";

export const convertData = (payments: Payment[]): TextData[] =>
  payments.map((payment) => ({
    type: "text",
    text: `$${payment.amount.toFixed(2)}`,
    helpText:
      payment.achinfo === null
        ? undefined
        : `Initiated: ${
            payment.createdat.getMonth() + 1
          }/${payment.createdat.getDate()}/${payment.createdat.getFullYear()}\nAccount: •••• ${payment.achinfo[1].slice(
            -4
          )}`,
    // eslint-disable-next-line react/display-name
    icon: (props) => <PaymentPill {...props} payment={payment} />,
    key: payment.id,
  }));
