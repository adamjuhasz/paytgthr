import React from "react";
import {
  Route,
  Switch,
  useHistory,
  useLocation,
} from "../../PlatformSpecific/react-router";
import { startsWith } from "lodash";
import { useSelector } from "react-redux";

import { State } from "../../State/State";
import CreditCard from "../Icons/CreditCard";
import Gear from "../Icons/Gear";
import Bank from "../Icons/Bank";
import LightningBolt from "../Icons/LightningBolt";
import Diamond from "../Icons/Diamond";
import BottomBar from "../BottomBar/BottomBar";
import TrackScreen from "../TrackScreen";

import SettingsHOC from "../MainScreen/Settings/SettingsHOC";
import SettingsDemo from "../MainScreen/Settings/SettingsDemo";
import PurchasesHOC from "../MainScreen/Purchases/PurchasesHOC";
import PurchasesDemo from "../MainScreen/Purchases/PurchasesDemo";
import PaymentsHOC from "../MainScreen/Payments/PaymentsHOC";
import PaymentsDemo from "../MainScreen/Payments/PaymentsDemo";
import DynamicDashboard from "../MainScreen/DynamicDashboard/DynamicDashboard";
import SetupDashboardDemo from "../MainScreen/SetupDashboard/DashboardDemo";
import SettingsPaths from "./SettingsRouter/Paths";
import StoreReview from "../StoreReviewRequest/StoreReview";
import RewardsHOC from "../MainScreen/Rewards/RewardsHOC";
import RewardsDemo from "../MainScreen/Rewards/RewardsDemo";
import BoostActivator from "../MainScreen/Rewards/BoostActivatorHOC";

import { path as getReferralCodePath } from "../Referrals/GetCodeHOC";
import { path as linkCodePath } from "../Referrals/LinkCodeHOC";

import { paths as introPaths } from "./IntroRouter";
import pathForStep from "./SignUpRouter/Paths";

import {
  boostActivationPath,
  dashboardPath,
  paymentsPath,
  rewardsPath,
  settingsPath,
  transactionsPath,
} from "./MainScreenRouter/Paths";

type BottomSelection =
  | "Home"
  | "Rewards"
  | "Purchases"
  | "Payments"
  | "Settings";

const MainScreenRouter = (): JSX.Element => {
  const history = useHistory();
  const location = useLocation();
  const { loggedIn } = useSelector((state: State) => ({
    loggedIn: state.userInfo.loggedIn,
    seenIntro: state.seenIntro,
  }));

  let step: BottomSelection = "Home";
  if (startsWith(location.pathname, rewardsPath)) {
    step = "Rewards";
  } else if (startsWith(location.pathname, transactionsPath)) {
    step = "Purchases";
  } else if (location.pathname === paymentsPath) {
    step = "Payments";
  } else if (location.pathname === settingsPath) {
    step = "Settings";
  }

  return (
    <BottomBar
      pages={[
        {
          text: "Home",
          icon: LightningBolt,
          selected: step === "Home",
          action: () => {
            history.push(dashboardPath);
          },
        },
        {
          text: "Rewards",
          icon: Diamond,
          selected: step === "Rewards",
          action: () => {
            history.push(rewardsPath);
          },
        },
        {
          text: "Purchases",
          icon: CreditCard,
          selected: step === "Purchases",
          action: () => {
            history.push(transactionsPath);
          },
        },
        {
          text: "Payments",
          icon: Bank,
          selected: step === "Payments",
          action: () => {
            history.push(paymentsPath);
          },
        },
        {
          text: "Settings",
          icon: Gear,
          selected: step === "Settings",
          action: () => {
            history.push(settingsPath);
          },
        },
      ]}
    >
      <Switch>
        <Route path={settingsPath}>
          <TrackScreen screen="MainScreen Settings" />
          {loggedIn ? (
            <SettingsHOC
              gotoDashboad={() => history.push(dashboardPath)}
              gotoGetReferralCode={() => history.push(getReferralCodePath)}
              gotoEnterReferralCode={() => history.push(linkCodePath)}
            />
          ) : (
            <SettingsDemo />
          )}
        </Route>

        <Route path={transactionsPath}>
          <TrackScreen screen="MainScreen Transactions" />
          {loggedIn ? (
            <>
              <StoreReview />
              <PurchasesHOC
                gotoActivateCard={(cid: string) =>
                  history.push(SettingsPaths.activateCardPath(cid))
                }
                gotoSetCardPin={(cid: string) =>
                  history.push(SettingsPaths.setCardPinPath(cid))
                }
              />
            </>
          ) : (
            <PurchasesDemo />
          )}
        </Route>

        <Route exact path={rewardsPath}>
          <TrackScreen screen="MainScreen Rewards" />
          {loggedIn ? (
            <>
              <StoreReview />
              <RewardsHOC
                gotoActivator={(level) =>
                  history.push(boostActivationPath(level))
                }
              />
            </>
          ) : (
            <RewardsDemo
              gotoActivator={(level) =>
                history.push(boostActivationPath(level))
              }
            />
          )}
        </Route>

        <Route path={boostActivationPath(":level")}>
          <TrackScreen screen="MainScreen BoostActivation" />
          <BoostActivator
            goNext={() => history.push(rewardsPath)}
            goBack={() => history.goBack()}
          />
        </Route>

        <Route path={paymentsPath}>
          <TrackScreen screen="MainScreen Payments" />
          {loggedIn ? <PaymentsHOC /> : <PaymentsDemo />}
        </Route>

        <Route path="/">
          <TrackScreen screen="MainScreen Dashboard" />
          {loggedIn ? (
            <>
              <DynamicDashboard
                gotoPayment={() => history.push(paymentsPath)}
                gotoPurhcase={() => history.push(transactionsPath)}
                gotoInviteCode={() =>
                  history.push(SettingsPaths.viewInviteCode)
                }
                gotoAcceptCode={() => history.push(SettingsPaths.acceptInvite)}
                gotoLinkBank={() => history.push(SettingsPaths.link)}
                gotoVerifyBank={() =>
                  history.push(SettingsPaths.manualLinkVerify)
                }
                gotoIntro={() => history.push(introPaths.start)}
                gotoSignup={() => history.push(pathForStep.start)}
              />
            </>
          ) : (
            <SetupDashboardDemo
              gotoIntro={() => history.push(introPaths.start)}
              gotoSignup={() => history.push(pathForStep.start)}
            />
          )}
        </Route>
      </Switch>
    </BottomBar>
  );
};

export default MainScreenRouter;
