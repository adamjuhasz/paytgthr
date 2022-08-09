import React from "react";
import { Route, Switch, useHistory } from "../../PlatformSpecific/react-router";

import pathForStep from "./SignUpRouter/Paths";
import TrackScreen from "../TrackScreen";
import PhoneEntry from "../SignUp/PhoneEntry/PhoneEntryHOC";
import PhoneVerify from "../SignUp/PhoneVerify/PhoneVerifyHOC";
import Name from "../SignUp/Name/NameHOC";
import Address from "../SignUp/Address/AddressHOC";
import DOBEntry from "../SignUp/DateOfBirth/DOBEntryHOC";
import SSNEntry from "../SignUp/SSN/SSNEntryHOC";
import LegalConsent from "../SignUp/LegalConsent/LegalConsentHOC";
import KYC, {
  KYCDelay,
  KYCFailed,
  KYCManual,
  KYCProcessing,
} from "../SignUp/KYC/KYC";
import Survey from "../SignUp/Survey/SurveryHOC";
import SignUpForm from "../SignupForm/SignUpFormHOC";
import UserStateRouter from "./UserStateRouter";
import { dashboardPath } from "./MainScreenRouter/Paths";

import NoRouteMatch from "../NoRouteMatch/NoRouteMatchHOC";

const SignUpRouter = (): JSX.Element => {
  const history = useHistory();

  return (
    <Switch>
      <Route exact path={pathForStep.start}>
        <UserStateRouter push={false} />
      </Route>

      <Route exact path={pathForStep.createAccount}>
        <TrackScreen screen="Signup" />
        <SignUpForm
          goBack={() => history.push(dashboardPath)}
          screenIndex={0}
          screenCount={7}
        />
      </Route>

      <Route exact path={pathForStep.phoneEntry}>
        <TrackScreen screen="SignUp PhoneEntry" />
        <PhoneEntry
          screenIndex={1}
          screenCount={7}
          goBack={() => history.push(dashboardPath)}
        />
      </Route>
      <Route exact path={pathForStep.phoneVerify}>
        <TrackScreen screen="SignUp PhoneVerify" />
        <PhoneVerify
          gotoChangePhone={() => history.push(pathForStep.phoneEntry)}
          screenIndex={1}
          screenCount={7}
          goBack={() => history.push(dashboardPath)}
        />
      </Route>
      <Route exact path={pathForStep.nameEntry}>
        <TrackScreen screen="SignUp Name" />
        <Name
          screenIndex={2}
          screenCount={7}
          goBack={() => history.push(dashboardPath)}
        />
      </Route>
      <Route exact path={pathForStep.legalAgree}>
        <TrackScreen screen="SignUp LegalConsent" />
        <LegalConsent
          screenIndex={3}
          screenCount={7}
          goBack={() => history.push(dashboardPath)}
        />
      </Route>
      <Route exact path={pathForStep.addressEntry}>
        <TrackScreen screen="SignUp Address" />
        <Address
          screenIndex={4}
          screenCount={7}
          goBack={() => history.push(dashboardPath)}
        />
      </Route>
      <Route exact path={pathForStep.dobEntry}>
        <TrackScreen screen="SignUp DOBEntry" />
        <DOBEntry
          screenIndex={5}
          screenCount={7}
          goBack={() => history.push(dashboardPath)}
        />
      </Route>
      <Route exact path={pathForStep.ssnEntry}>
        <TrackScreen screen="SignUp PII" />
        <SSNEntry
          gotoNextScreen={() => history.push(pathForStep.survey)}
          screenIndex={6}
          screenCount={7}
          goBack={() => history.push(dashboardPath)}
        />
      </Route>
      <Route exact path={pathForStep.survey}>
        <TrackScreen screen="SignUp Survey" />
        <Survey gotoNextScreen={() => history.push(pathForStep.kyc)} />
      </Route>
      <Route exact path={pathForStep.kyc}>
        <TrackScreen screen="SignUp KYC" />
        <KYC />
      </Route>
      <Route exact path={pathForStep.kycDelay}>
        <TrackScreen screen="SignUp KYC TempDelay" />
        <KYCDelay />
      </Route>
      <Route exact path={pathForStep.kycManual}>
        <TrackScreen screen="SignUp KYC ManualNeeded" />
        <KYCManual />
      </Route>
      <Route exact path="/app/signup/kyc/failed">
        <TrackScreen screen="SignUp KYC Failed" />
        <KYCFailed />
      </Route>
      <Route exact path="/app/signup/kyc/processing">
        <TrackScreen screen="SignUp KYC Processing" />
        <KYCProcessing />
      </Route>

      {/* Match error */}
      <Route>
        <TrackScreen screen="SignUp RoutingFail" />
        <NoRouteMatch router="SignUpRouter" />
      </Route>
    </Switch>
  );
};

export default SignUpRouter;
