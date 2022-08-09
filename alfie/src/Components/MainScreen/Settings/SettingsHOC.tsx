import React from "react";
import { Linking, Platform } from "react-native";
import { useHistory } from "../../../PlatformSpecific/react-router";
import { useDispatch, useSelector } from "react-redux";
import Alert from "../../../PlatformSpecific/Alert";
import { MutateFunction, useMutation, useQueryCache } from "react-query";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";
import { defaultTo } from "lodash";

import { AlfieDispatch } from "../../../State/Store";
import { State } from "../../../State/State";
import { logout } from "../../../Actions/Logout";
import { SettingsPaths } from "../../Routers/SettingsRouter/Paths";
import { submitRequestStatement } from "../../../Actions/User/RequestStatement";
import { EnhancedCardModel } from "../../../Actions/Card/Types";
import useGetUser, { queryPath as getUserPath } from "../../Hooks/UseGetUser";
import { closeGroup as sendCloseAction } from "../../../Actions/Groups/CloseGroup";
import { UserState } from "../../../Types/UserStateTypes";
import { sendEmail } from "../../../Helpers/SendEmail";
import { forgotPasswordPath } from "../../Routers/Password";
import { loginPath } from "../../Routers/RootRouter/Paths";
import signupPaths from "../../Routers/SignUpRouter/Paths";
import useGetCards from "../../Hooks/UseGetCards";
import { useLockCard, useUnlockCard } from "../../Hooks/UseChangeCard";
import { Inputs } from "../../../Actions/Card/CreateCard";
import { useCreateCard } from "../../Hooks/UseCreateCard";
import useGetLevel from "../../Hooks/UseGetLevel";
import { useGetRefereesProgress } from "../../../Actions/Referral/GetRefereeProgress";
import { useGetMyReferralProgress } from "../../../Actions/Referral/GetReferralProgress";

import Settings from "./Settings";

const orderPhysicalCard = (
  userstate: UserState | null,
  cards: EnhancedCardModel[],
  createCard: MutateFunction<void, unknown, Inputs, unknown>
) => {
  if (userstate === null) {
    return;
  }

  if (userstate.user.ach.verified !== true) {
    //user is not verified
    void Analytics.track("User PhysicalCard LinkFailure");
    Alert.alert(
      "You'll need to link your bank first",
      "Before we can ship you a card, you'll need to link and verify a bank account"
    );
    return;
  }

  if (userstate.partner?.verified !== true) {
    //user is not verified
    void Analytics.track("User PhysicalCard PartnerFailure");
    Alert.alert(
      "Partner needs to link a bank account",
      "Before we can ship you a card, they'll need to link a bank account"
    );
    return;
  }

  const physicalCards = cards.filter(
    (c) =>
      c.cardDesign === "PhysicalBlack" &&
      c.cardPlatform.tag === "PayWithPrivacy"
  );

  if (physicalCards.length > 0) {
    void Analytics.track("User PhysicalCard ReorderFailure");
    Alert.alert(
      "You'll need to email us",
      "Please contact customer service about ordering a replacement physical card"
    );
    return;
  }

  const frozenCards = cards.filter((c) => c.cardStatus === "ADMINFROZEN");
  if (frozenCards.length > 0) {
    void Analytics.track("User PhysicalCard FrozenFailure");
    Alert.alert(
      "Please contact support",
      "We hit a snag ordering your card... can you email us at hi@paytgthr.com"
    );
    return;
  }

  void Analytics.track("User PhysicalCard Tapped");
  const addressLine1 = `${userstate.user.address.street || "no street found"}`;
  const addresLine2 = `${userstate.user.address.city || "no city found"}, ${
    userstate.user.address.state || "no state found"
  } ${userstate.user.address.zip || "no zip found"}`;

  Alert.alert(
    "Have you moved?",
    `We'll send the card to the address on file, please make sure it's correct:\n\n${addressLine1}\n${addresLine2}`,
    [
      {
        text: "Order",
        onPress: () => {
          void Analytics.track("User PhysicalCard Ordered");
          void createCard({ type: "PhysicalBlack" });
        },
        style: "default",
      },
      {
        text: "Cancel",
        onPress: () => {
          void Analytics.track("User PhysicalCard Cancelled");
        },
        style: "cancel",
      },
    ]
  );
};

const requestPLIncrease = (userstate: UserState | null) => () => {
  if (userstate !== null && userstate.user.ach.verified !== true) {
    //user is not verified
    void Analytics.track("User RequestIncrease LinkFailure");
    Alert.alert(
      "You'll need to link first",
      "Before we can increase your limit, you'll need to link and verify a bank account"
    );
    return;
  }

  void Analytics.track("User RequestIncrease Tapped");
  Alert.alert(
    "Bank statement required",
    "Send us the 2 most recent statements for your linked bank account to increase your spending limit\n\nhi@paytgthr.com",
    [
      {
        text: "Send email",
        onPress: () => {
          void Analytics.track("User RequestIncrease SendEmail");
          void sendEmail(
            "Bank statements attached for spending limit increase",
            "Attach your 2 most recent bank statements below"
          );
        },
        style: "default",
      },
      {
        text: "Cancel",
        onPress: () => {
          void Analytics.track("User RequestIncrease Cancelled");
        },
        style: "cancel",
      },
    ]
  );
};

interface Props {
  gotoDashboad: () => void;
  gotoGetReferralCode: () => void;
  gotoEnterReferralCode: () => void;
}

const SettingsHOC = (props: Props): JSX.Element => {
  const { baseURL, loggedIn } = useSelector((state: State) => ({
    loggedIn: state.userInfo.loggedIn,
    baseURL: state.baseURL,
  }));
  const dispatch = useDispatch<AlfieDispatch>();
  const history = useHistory();
  const cache = useQueryCache();

  const cardsQuery = useGetCards();

  const userState = useGetUser();

  const [lockACard] = useLockCard();
  const [unlockACard] = useUnlockCard();

  const [closeAGroup] = useMutation(sendCloseAction(baseURL), {});

  const [createCard] = useCreateCard();

  const levels = useGetLevel();

  const referees = useGetRefereesProgress();
  const myProgressQuery = useGetMyReferralProgress();

  /* Hoooks done */

  let cards: EnhancedCardModel[] = [];
  if (cardsQuery.data !== undefined && cardsQuery.data.length > 0) {
    cards = cardsQuery.data
      .filter((c) => c.cardPlatform.tag === "PayWithPrivacy")
      .filter((c) => c.cardStatus !== "CLOSED");
  }

  const requestStatement = async () => {
    void Analytics.track("User Statement Requested");
    await submitRequestStatement(baseURL);
    Alert.alert("Your statement was emailed to you");
  };

  const callUs = () => {
    void Analytics.track("User CustomerSupport Requested", { medium: "phone" });
    Linking.openURL("tel:+12341231234").catch(() => true);
  };

  const emailUs = () => {
    void Analytics.track("User CustomerSupport Requested", { medium: "email" });
    Linking.openURL("mailto:hi@paytgthr.com").catch(() => true);
  };

  const logoutUser = async () => {
    void Analytics.track("User SettingsLogout Clicked", {
      from: "SettingsHOC",
    });
    await dispatch(logout());
    props.gotoDashboad();
  };

  let closeGroup = () => {
    Alert.alert("Error, can you try again?");
  };

  let changeSplit = () => {
    Alert.alert("Error, can you try again?");
  };

  let gid: null | string = null;

  if (userState === null) {
    gid = null;
  }

  let changeFundingSource = () => {
    Alert.alert("You'll need to verify your identity before you can do this");
  };

  if (userState !== null && userState.group.id != null) {
    const aGid = userState.group.id;

    gid = aGid;

    closeGroup = () => {
      Alert.alert(
        "Close group?",
        "This will close your group immediately, you'll need to re-invite a partner to use your Tgthr Card",
        [
          {
            text: "Close group",
            style: "destructive",
            onPress: async () => {
              await closeAGroup(aGid);
              void Analytics.track("User Group Closed");
              void cache.invalidateQueries([getUserPath]);
            },
          },
          {
            text: "Cancel",
            style: "cancel",
            onPress: () => ({}),
          },
        ]
      );
    };

    changeSplit = () => {
      void Analytics.track("User Group ChangeSplit Request");
      history.push(SettingsPaths.ratio(aGid));
    };
  }

  if (userState !== null && userState.user.status.tag === "UserActive") {
    changeFundingSource = () => {
      history.push(SettingsPaths.fsList);
    };
  }

  const addCardToDW = () => {
    if (
      userState?.user.ach.verified === true &&
      userState.partner?.verified === true
    ) {
      switch (Platform.OS) {
        case "ios":
          void Analytics.track("Digital Wallet Add");
          void Linking.openURL("wallet://payment_setup");
          break;

        case "android":
          void Analytics.track("Digital Wallet Add");
          void Linking.openURL("https://gpay.app.goo.gl/kHXsJi");
          break;

        default:
          break;
      }
    } else {
      Alert.alert(
        "Bank accounts need to be linked",
        "Before you can use your card both you and your partner have to link a bank account"
      );
    }
  };

  const createdAt = defaultTo(userState?.user?.timestamps?.created, null);
  const now = new Date();
  const hoursInMs = 1000 * 60 * 60;
  const hoursSinceCreated: number =
    createdAt === null
      ? 9999
      : (now.valueOf() - new Date(createdAt).valueOf()) / hoursInMs;

  const alreadyEnteredCode =
    myProgressQuery.data === undefined
      ? true
      : myProgressQuery.data === null
      ? false
      : true;
  const allowRefCodeEntry = hoursSinceCreated !== 0 && !alreadyEnteredCode;

  return (
    <Settings
      hasGroup={gid}
      closeGroup={closeGroup}
      changeSplit={changeSplit}
      changeFS={changeFundingSource}
      callCS={callUs}
      emailCS={emailUs}
      requestStatement={requestStatement}
      logout={logoutUser}
      lockCard={lockACard}
      unlockCard={unlockACard}
      cards={cards}
      orderPhysical={() => orderPhysicalCard(userState, cards, createCard)}
      gotoViewInviteCode={() => history.push(SettingsPaths.viewInviteCode)}
      gotoAcceptInvite={() => history.push(SettingsPaths.acceptInvite)}
      requestPLIncrease={requestPLIncrease(userState)}
      loggedIn={loggedIn}
      login={() => history.push(loginPath)}
      resetPassword={() => history.push(forgotPasswordPath)}
      gotoSignup={() => history.push(signupPaths.createAccount)}
      loadingCards={cardsQuery.data === undefined}
      currentProgress={levels.data.currentProgress}
      dollarsToNextLevel={levels.data.dollarsToNextLevel}
      addCardToDigitalWallet={addCardToDW}
      gotoGetReferralCode={
        levels.data.currentLevel > 1 ? props.gotoGetReferralCode : undefined
      }
      gotoEnterReferralCode={
        allowRefCodeEntry ? props.gotoEnterReferralCode : undefined
      }
      refereeProgress={defaultTo(referees.data, [])}
    />
  );
};

export default SettingsHOC;
