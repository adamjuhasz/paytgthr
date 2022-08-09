import React, { useEffect } from "react";
import { hydrate, render } from "react-dom";
import {
  Route,
  BrowserRouter as Router,
  Routes,
  useLocation,
} from "react-router-dom";
import { QueryClient, QueryClientProvider } from "react-query";
import { broadcastQueryClient } from "react-query/broadcastQueryClient-experimental";
import { persistQueryClient } from "react-query/persistQueryClient-experimental";
import { createWebStoragePersistor } from "react-query/createWebStoragePersistor-experimental";

import "./index.css";

import { Helmet } from "react-helmet";

import "@fontsource/rubik/300.css";
import "@fontsource/rubik/400.css";
import "@fontsource/rubik/500.css";
import "@fontsource/rubik/600.css";
import "@fontsource/rubik/700.css";
import "@fontsource/rubik/800.css";
import "@fontsource/rubik/900.css";
import "@fontsource/rubik/variable.css";

import RefreshNotification from "./RefreshNotification";

import { getServerLocation } from "./Backend/Server";
import FourOhFour from "./404/404";
import GlobalWeeklyReport, {
  path as weeklyReportPath,
} from "./Reports/GlobalWeeklyReport";
import SidebarNav from "./Shared/SidebarNav";
import AppEventsForUser, {
  path as appUserEventsPath,
} from "./AppEvents/AppEventsForUser";
import AppEventsForDevie, {
  path as appDeviceEventsPath,
} from "./AppEvents/AppEventsForDevice";
import AppEventDevices, { path as appDevicesPath } from "./AppEvents/Devices";
import UserView, { path as userViewPath } from "./User/UserView";
import UserWeeklyReport, {
  path as userWeeklyReportPath,
} from "./Reports/UserWeeklyReport";
import Rewards, { path as rewardsPath } from "./Reports/Rewards";
import ListBoosts, { path as listBoostsPath } from "./Rewards/ListBoosts";
import CreateNewBoost, {
  path as createNewBoostPath,
} from "./Rewards/CreateNew";
import BoostDetail, { path as boostDetailPath } from "./Rewards/BoostDetail";
import SpecificPurchase, {
  path as purchasePath,
} from "./Purchases/SpecificPurchase";
import PaymentDetails, {
  path as paymentDetailsPath,
} from "./Payments/PaymentDetails";
import AllPayments, { path as allPaymentsPath } from "./Payments/AllPayments";
import ScheduledPayments, {
  path as scheduledPaymentsPath,
} from "./Payments/ScheduledPayments";
import LedgerRouter from "./Ledger/Router";
import UsersRouter from "./Users/Router";
import RefRouter from "./Referrals/Router";
import ChurnReport, { path as churnReportPath } from "./Reports/Churn";
import PurchaseReport, {
  path as purchaseReportPath,
} from "./Reports/Purchases";
import UsersReport, { path as usersReporPath } from "./Reports/Users";
import ShowAllInvites from "./PartnerInvites/ShowAll";

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      cacheTime: 1000 * 60, //1 min
    },
  },
});

const isolatedQC = new QueryClient();

broadcastQueryClient({
  queryClient,
  broadcastChannel: "chewpaca-react-query",
});

const localStoragePersistor = createWebStoragePersistor({
  storage: window.localStorage,
});

void persistQueryClient({
  queryClient,
  persistor: localStoragePersistor,
});

const ScrollToTop = () => {
  const { pathname, hash } = useLocation();

  React.useEffect(() => {
    window.scrollTo(0, 0);
    document
      .getElementById("route-container")
      ?.scrollTo({ top: 0, left: 0, behavior: "auto" });
  }, [pathname, hash]);

  return <></>;
};

const App = () => {
  useEffect(() => {
    getServerLocation();
  }, []);

  return (
    <React.StrictMode>
      <Helmet>
        <title>Chewpaca</title>
      </Helmet>

      <QueryClientProvider client={queryClient}>
        <Router>
          <ScrollToTop />
          <div className="h-full flex">
            <SidebarNav />
            <div className="flex-1 overflow-y-scroll" id="route-container">
              <Routes>
                <Route path="/" element={<div>hi</div>} />
                <Route path="/ledger/*" element={<LedgerRouter />} />
                <Route path="/users/*" element={<UsersRouter />} />
                <Route path="/referrals/*" element={<RefRouter />} />
                <Route
                  path={weeklyReportPath}
                  element={<GlobalWeeklyReport />}
                />
                <Route path={rewardsPath} element={<Rewards />} />
                <Route
                  path={appUserEventsPath}
                  element={<AppEventsForUser />}
                />
                <Route
                  path={appDeviceEventsPath}
                  element={<AppEventsForDevie />}
                />
                <Route path={appDevicesPath} element={<AppEventDevices />} />
                <Route path={userViewPath} element={<UserView />} />
                <Route
                  path={userWeeklyReportPath}
                  element={<UserWeeklyReport />}
                />
                <Route path={listBoostsPath} element={<ListBoosts />} />
                <Route path={createNewBoostPath} element={<CreateNewBoost />} />
                <Route path={boostDetailPath} element={<BoostDetail />} />

                <Route path={purchasePath} element={<SpecificPurchase />} />

                <Route path={paymentDetailsPath} element={<PaymentDetails />} />
                <Route path={allPaymentsPath} element={<AllPayments />} />
                <Route
                  path={scheduledPaymentsPath}
                  element={<ScheduledPayments />}
                />
                <Route path={churnReportPath} element={<ChurnReport />} />
                <Route path={purchaseReportPath} element={<PurchaseReport />} />
                <Route path={usersReporPath} element={<UsersReport />} />

                <Route path={"/invites"} element={<ShowAllInvites />} />

                <Route path="*" element={<FourOhFour />} />
              </Routes>
            </div>
          </div>
        </Router>
      </QueryClientProvider>

      <QueryClientProvider client={isolatedQC}>
        <RefreshNotification />
      </QueryClientProvider>
    </React.StrictMode>
  );
};

const rootElement = document.getElementById("root");
if (rootElement !== null && rootElement.hasChildNodes()) {
  hydrate(<App />, rootElement);
} else {
  render(<App />, rootElement);
}
