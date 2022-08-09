import React from "react";
import { Linking } from "react-native";
import { useHistory } from "../../../PlatformSpecific/react-router";
import Alert from "../../../PlatformSpecific/Alert";

import { forgotPasswordPath } from "../../Routers/Password";
import { loginPath } from "../../Routers/RootRouter/Paths";
import { SettingsPaths } from "../../Routers/SettingsRouter/Paths";
import signupPaths from "../../Routers/SignUpRouter/Paths";

import Settings from "./Settings";

export default function SettingsDemo(_props: unknown): JSX.Element {
  const history = useHistory();

  const callUs = () => {
    Linking.openURL("tel:+12341231234").catch(() => true);
  };

  const emailUs = () => {
    Linking.openURL("mailto:hi@paytgthr.com").catch(() => true);
  };

  const alertNeedToBeLoggedIn = () => {
    Alert.alert("You need to verify your identity to do this");
  };

  return (
    <Settings
      hasGroup={null}
      closeGroup={alertNeedToBeLoggedIn}
      changeSplit={() =>
        history.push(
          SettingsPaths.ratio("00000000-0000-0000-0000-000000000000")
        )
      }
      changeFS={alertNeedToBeLoggedIn}
      callCS={callUs}
      emailCS={emailUs}
      requestStatement={alertNeedToBeLoggedIn}
      logout={alertNeedToBeLoggedIn}
      lockCard={alertNeedToBeLoggedIn}
      unlockCard={alertNeedToBeLoggedIn}
      cards={[]}
      orderPhysical={alertNeedToBeLoggedIn}
      gotoViewInviteCode={alertNeedToBeLoggedIn}
      gotoAcceptInvite={alertNeedToBeLoggedIn}
      requestPLIncrease={alertNeedToBeLoggedIn}
      loggedIn={false}
      login={() => history.push(loginPath)}
      resetPassword={() => history.push(forgotPasswordPath)}
      gotoSignup={() => history.push(signupPaths.createAccount)}
      loadingCards={false}
      currentProgress={1.0}
      dollarsToNextLevel={250}
      addCardToDigitalWallet={alertNeedToBeLoggedIn}
      gotoEnterReferralCode={undefined}
      gotoGetReferralCode={undefined}
      refereeProgress={[]}
    />
  );
}
