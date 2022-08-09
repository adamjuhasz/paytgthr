import React, { useContext } from "react";
import {
  Linking,
  Platform,
  SectionListData,
  StyleSheet,
  View,
} from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import Alert from "../../../PlatformSpecific/Alert";

import SectionedList from "../../SectionedList/SectionedList";
import { Data, Section } from "../../SectionedList/Types";
import { ThemeContext } from "../../Theming/ThemeContext";
import { EnhancedCardModel } from "../../../Actions/Card/Types";
import { ReferralProgress } from "../../../Actions/Referral/Types";

import GroupIcon from "../../Icons/Group";
import PieChart from "../../Icons/PieChart";
import BankIcon from "../../Icons/Bank";
import Locked from "../../Icons/Locked";
import Unlocked from "../../Icons/Unlocked";
// import PaperAirplane from "../../Icons/PaperAirplane";
import Envelope from "../../Icons/Envelope";
// import NumberPad from "../../Icons/NumberPad";
import TrafficCone from "../../Icons/TrafficCone";
import CreditCard from "../../Icons/CreditCard";
import Export from "../../Icons/Export";
import Link from "../../Icons/Link";
import BarGraph from "../../Icons/BarGraph";
import User from "../../Icons/User";
import Id from "../../Icons/Id";
import Star from "../../Icons/Star";
import Money from "../../Icons/Money";

import PLIncrease from "../../MainScreen/Purchases/PLIncrease";
import ReferallProgress from "./ReferralProgress";

interface Props {
  hasGroup: null | string;
  closeGroup: () => void;
  changeSplit: () => void;
  changeFS: () => void;
  callCS: () => void;
  emailCS: () => void;
  requestStatement: () => void;
  logout: () => void;
  cards: EnhancedCardModel[];
  lockCard: (cardId: string) => void;
  unlockCard: (cardId: string) => void;
  orderPhysical: () => void;
  gotoViewInviteCode: () => void;
  gotoAcceptInvite: () => void;
  requestPLIncrease: () => void;
  loggedIn: boolean;
  login: () => void;
  resetPassword: () => void;
  gotoSignup: () => void;
  loadingCards: boolean;
  currentProgress: number;
  dollarsToNextLevel: number;
  addCardToDigitalWallet: (cardId: string) => void;
  gotoGetReferralCode: undefined | (() => void);
  gotoEnterReferralCode: undefined | (() => void);
  refereeProgress: ReferralProgress[];
}

const Settings = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const insets = useSafeAreaInsets();

  const invitePartner: Data = {
    type: "text",
    text: "Invite partner",
    icon: Export,
    action: props.gotoViewInviteCode,
  };

  const acceptInvite: Data = {
    type: "text",
    text: "Accept an invite",
    icon: GroupIcon,
    action: props.gotoAcceptInvite,
  };

  const closeGroup: Data = {
    type: "text",
    text: "Close group",
    icon: GroupIcon,
    action: props.closeGroup,
  };

  const changeSplit: Data = {
    type: "text",
    text: "Change purchase splits",
    icon: PieChart,
    action: props.changeSplit,
  };

  let groupOptions: Data[] = [];
  if (props.hasGroup === null) {
    groupOptions = [invitePartner, acceptInvite];
  } else if (props.hasGroup === undefined) {
    groupOptions = [];
  } else {
    groupOptions = [closeGroup, changeSplit];
  }

  if (props.loggedIn === false) {
    groupOptions = [...groupOptions, changeSplit];
  }

  const loginSection: SectionListData<Data, Section>[] = props.loggedIn
    ? []
    : [
        {
          sectionType: "text",
          title: "Login",
          data: [
            {
              type: "text",
              text: "Login to existing account",
              icon: User,
              action: props.login,
            },
            {
              type: "text",
              text: "Reset password",
              icon: Id,
              action: props.resetPassword,
            },
            {
              type: "text",
              text: "Create an account",
              icon: Star,
              action: props.gotoSignup,
            },
          ],
        },
      ];
  const logoutSection: SectionListData<Data, Section>[] = props.loggedIn
    ? [
        {
          sectionType: "text",
          title: "Logout",
          data: [
            {
              type: "text",
              text: "Logout",
              icon: TrafficCone,
              action: props.logout,
            },
          ],
        },
      ]
    : [];

  let cardOptions: Data[] = [];

  if (props.cards.length === 1) {
    const card = props.cards[0];
    const cardId = card.cardId;
    switch (card.cardStatus) {
      case "CREATED":
        break;

      case "ACTIVATED":
        cardOptions = [
          {
            type: "text",
            text: "Lock card",
            icon: Unlocked,
            action: () => props.lockCard(cardId),
          },
          ...cardOptions,
        ];
        break;

      case "USERFROZEN":
        cardOptions = [
          {
            type: "text",
            text: "Unlock card",
            icon: Locked,
            action: () => props.unlockCard(cardId),
          },
          ...cardOptions,
        ];
        break;

      case "ADMINFROZEN":
        cardOptions = [
          {
            type: "text",
            text: "Unlock card",
            icon: Locked,
            action: () => {
              Alert.alert("Card could not be unlocked, please contact support");
            },
          },
          ...cardOptions,
        ];
        break;

      case "CLOSED":
        break;
    }
  }

  if (props.cards.length > 1) {
    cardOptions = [
      ...props.cards
        .filter(
          (c) => c.cardStatus === "ACTIVATED" || c.cardStatus === "USERFROZEN"
        )
        .map(
          (c): Data =>
            c.cardStatus === "ACTIVATED"
              ? {
                  type: "text",
                  text: `Lock card ending in ${c.pan.slice(-4)}`,
                  icon: Unlocked,
                  action: () => props.lockCard(c.cardId),
                }
              : {
                  type: "text",
                  text: `Unlock card ending in ${c.pan.slice(-4)}`,
                  icon: Unlocked,
                  action: () => props.unlockCard(c.cardId),
                }
        ),
      ...cardOptions,
    ];
  }

  if (props.cards.length > 0 && props.hasGroup !== null) {
    switch (Platform.OS) {
      case "ios":
        cardOptions = [
          {
            type: "text",
            text: "Add card to Apple Pay",
            icon: CreditCard,
            action: () => {
              props.addCardToDigitalWallet("");
            },
          },
          ...cardOptions,
        ];
        break;

      case "android":
        cardOptions = [
          {
            type: "text",
            text: "Add card to Google Pay",
            icon: CreditCard,
            action: () => {
              props.addCardToDigitalWallet("");
            },
          },
          ...cardOptions,
        ];
        break;

      default:
        break;
    }
  }

  if (props.loadingCards) {
    cardOptions = [];
  }

  const accountSettings: Data[] = [
    {
      type: "text",
      text: "Change bank account",
      icon: BankIcon,
      action: props.changeFS,
    },
    {
      type: "node",
      key: "pl increase",
      node: (
        <View style={[styles.plIncrease]}>
          <PLIncrease
            currentProgress={props.currentProgress}
            dollarsToNextLevel={props.dollarsToNextLevel}
          />
        </View>
      ),
    },
    {
      type: "text",
      text: "Request spending limit increase",
      icon: BarGraph,
      action: props.requestPLIncrease,
    },
  ];

  let refferralSettings: Data[] = [];
  const referralSectionName = "Refer a friend";

  if (props.gotoGetReferralCode !== undefined) {
    refferralSettings = [
      ...refferralSettings,
      {
        type: "text",
        text: "Share your referral code",
        icon: Money,
        action: props.gotoGetReferralCode,
      },
    ];
  }

  if (props.gotoEnterReferralCode !== undefined) {
    refferralSettings = [
      ...refferralSettings,
      {
        type: "text",
        text: "Enter friend's referral code",
        icon: Money,
        action: props.gotoEnterReferralCode,
      },
    ];
  }

  refferralSettings = [
    ...refferralSettings,
    ...props.refereeProgress.map((prog) => {
      const node: Data = {
        type: "node",
        key: prog.created,
        node: (
          <View style={[styles.plIncrease]}>
            <ReferallProgress progress={prog} />
          </View>
        ),
      };
      return node;
    }),
  ];

  let referralSection: SectionListData<Data, Section>[] = [];
  if (refferralSettings.length > 0) {
    referralSection = [
      {
        sectionType: "text",
        title: referralSectionName,
        data: refferralSettings,
      },
    ];
  }

  return (
    <View style={styles.container}>
      <theme.background style={[StyleSheet.absoluteFill]} />
      <theme.background style={[styles.topInset, { height: insets.top }]} />
      <SectionedList
        itemStyle={[styles.sectionlist]}
        sectionStyle={[styles.sectionlist]}
        data={[
          ...referralSection,
          ...loginSection,
          {
            sectionType: "text",
            title: "Couple settings",
            data: groupOptions,
          },
          {
            sectionType: "text",
            title: "Account settings",
            data: accountSettings,
          },
          {
            sectionType: "text",
            title: "Pay Tgthr",
            data: cardOptions,
          },
          {
            sectionType: "text",
            title: "Get help",
            data: [
              // {
              //   type: "text",
              //   text: "Text support",
              //   icon: PaperAirplane,
              //   action: props.smsCS,
              // },
              {
                type: "text",
                text: "Email support",
                icon: Envelope,
                action: props.emailCS,
              },
              // {
              //   type: "text",
              //   text: "Call support",
              //   icon: NumberPad,
              //   action: props.callCS,
              // },
            ],
          },
          {
            sectionType: "text",
            title: "Lawyer stuff",
            data: [
              {
                type: "text",
                text: "Legal agreements",
                icon: Link,
                action: () => {
                  void Linking.openURL("https://paytgthr.com/#legal");
                },
              },
            ],
          },
          ...logoutSection,
        ]}
      />
    </View>
  );
};
Settings.displayName = "Settings";

export default Settings;

const styles = StyleSheet.create({
  container: { flex: 1 },
  plIncrease: { marginBottom: 10, paddingHorizontal: 10 },
  sectionlist: { marginHorizontal: 0 },
  topInset: { width: "100%" },
});
