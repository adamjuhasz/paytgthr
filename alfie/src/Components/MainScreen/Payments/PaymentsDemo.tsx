import React from "react";

import Payments from "./Payments";

export default function PaymentsDemo(): JSX.Element {
  return (
    <Payments
      payments={[]}
      pendingPurchases={0}
      upcomingPayments={[]}
      credits={[]}
      demoMode={true}
    />
  );
}
