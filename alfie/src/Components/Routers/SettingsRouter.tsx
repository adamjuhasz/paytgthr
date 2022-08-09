import React from "react";
import { Route, Switch, useHistory } from "../../PlatformSpecific/react-router";

import TrackScreen from "../TrackScreen";
import LinkTypeChooser from "../FundingSource/LinkTypeChooser";
import PlaidLink from "../FundingSource/ACH/Plaid/PlaidHOC";
import ManualLink from "../FundingSource/ACH/Manual/ManualLink/ManualLinkHOC";
import ManalLinkWaiting from "../FundingSource/ACH/Manual/ManualLinkWaiting/ManalLinkWaitingHOC";
import VerifyManualLink from "../FundingSource/ACH/Manual/VerifyManual/VerifyManualLinkHOC";
import NoRouteMatch from "../NoRouteMatch/NoRouteMatchHOC";
import GroupSplit from "../Group/GroupSplit/GroupSplitHOC";
import FundingSourceList from "../MainScreen/Settings/FundingSources/FundingSourceListHOC";
import ViewInviteCodeHOC from "../Invite/ViewInviteCodeHOC";
import AcceptInviteHOC from "../Invite/AcceptInviteHOC";
import ActivateCard from "../ActivateCard/ActivateCardHOC";
import SetCardPin from "../SetCardPin/SetCardPinHOC";

import SettingsPaths from "./SettingsRouter/Paths";
import * as MainPaths from "./MainScreenRouter/Paths";

const Settingsrouter = (): JSX.Element => {
  const history = useHistory();

  return (
    <Switch>
      {/* Bank Linking */}
      <Route exact path={SettingsPaths.fsList}>
        <TrackScreen screen="Settings FSList" />
        <FundingSourceList
          goBack={() => history.push(SettingsPaths.settings)}
          gotoVerify={() => history.push(SettingsPaths.manualLinkVerify)}
          gotoAddNew={() => history.push(SettingsPaths.link)}
        />
      </Route>
      <Route exact path={SettingsPaths.link}>
        <TrackScreen screen="Settings LinkChooser" />
        <LinkTypeChooser
          goBack={() => history.push(SettingsPaths.settings)}
          chooseManual={() => history.push(SettingsPaths.manualLink)}
          choosePlaid={() => history.push(SettingsPaths.plaidLink)}
        />
      </Route>

      <Route exact path={SettingsPaths.plaidLink}>
        <TrackScreen screen="Settings PlaidLink" />
        <PlaidLink
          goBack={() => history.push(SettingsPaths.link)}
          goNext={() => history.push(SettingsPaths.settings)}
        />
      </Route>

      {/* Manual Link*/}
      <Route exact path={SettingsPaths.manualLink}>
        <TrackScreen screen="Settings ManualLink" />
        <ManualLink
          gotoNext={() => history.replace(SettingsPaths.manualLinkWait)}
        />
      </Route>
      <Route exact path={SettingsPaths.manualLinkWait}>
        <TrackScreen screen="Settings ManualLinkWaiting" />
        <ManalLinkWaiting
          gotoNext={() => history.replace(SettingsPaths.dashboard)}
        />
      </Route>
      <Route exact path={SettingsPaths.manualLinkVerify}>
        <TrackScreen screen="Settings ManualLinkVerify" />
        <VerifyManualLink />
      </Route>

      <Route path={SettingsPaths.ratio(":gid")}>
        <TrackScreen screen="Settings Change Split" />
        <GroupSplit
          navigation={{
            type: "action",
            action: () => history.push(SettingsPaths.settings),
          }}
          goBack={() => history.push(SettingsPaths.settings)}
          screenIndex={0}
          screenCount={1}
        />
      </Route>

      <Route exact path={SettingsPaths.viewInviteCode}>
        <TrackScreen screen="Settings ViewInviteCode" />
        <ViewInviteCodeHOC
          goBack={() => history.push(SettingsPaths.settings)}
        />
      </Route>

      <Route exact path={SettingsPaths.acceptInvite}>
        <TrackScreen screen="Settings AcceptInvite" />
        <AcceptInviteHOC goBack={() => history.push(SettingsPaths.settings)} />
      </Route>

      <Route exact path={SettingsPaths.activateCardPath(":cardid")}>
        <TrackScreen screen="CardScreen ActivateCard" />
        <ActivateCard
          goBack={() => history.push(MainPaths.transactionsPath)}
          goNext={(cardId: string) =>
            history.push(SettingsPaths.setCardPinPath(cardId))
          }
        />
      </Route>

      <Route exact path={SettingsPaths.setCardPinPath(":cardid")}>
        <TrackScreen screen="CardScreen SetPin" />
        <SetCardPin
          goBack={() => history.push(MainPaths.transactionsPath)}
          goNext={() => history.push(MainPaths.transactionsPath)}
        />
      </Route>

      {/* no match */}
      <Route>
        <TrackScreen screen="Settingsrouter NoMatch" />
        <NoRouteMatch router="Settingsrouter" />
      </Route>
    </Switch>
  );
};

export default Settingsrouter;
