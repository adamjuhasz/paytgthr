import React from "react";
import { Route, Switch, useHistory } from "../../PlatformSpecific/react-router";

import TrackScreen from "../TrackScreen";

import GetReferralCode, {
  path as getReferralCodePath,
} from "../Referrals/GetCodeHOC";
import LinkReferralCode, {
  path as linkReferralCodePath,
} from "../Referrals/LinkCodeHOC";

import { settingsPath } from "./MainScreenRouter/Paths";

export default function ReferralsRouter(): JSX.Element {
  const history = useHistory();

  return (
    <Switch>
      <Route exact path={getReferralCodePath}>
        <TrackScreen screen="Referral Getcode" />
        <GetReferralCode goBack={() => history.push(settingsPath)} />
      </Route>

      <Route exact path={linkReferralCodePath}>
        <TrackScreen screen="Referral LinkCode" />
        <LinkReferralCode
          goBack={() => history.push(settingsPath)}
          gotoNext={() => history.push(settingsPath)}
        />
      </Route>
    </Switch>
  );
}
