import React from "react";
import { Text, TouchableOpacity } from "react-native";
import { useHistory } from "../../../../PlatformSpecific/react-router";
import Analytics from "../../../../PlatformSpecific/SegmentAnalytcs";

import { ACHState } from "../../../../Types/UserStateTypes";
import Banner, { fontSize } from "../../../Banner/Banner";
import TextStyles from "../../../Styling/Text";
import styles from "./Styles";
import useGetUser from "../../../Hooks/UseGetUser";
import SettingsPaths from "../../../Routers/SettingsRouter/Paths";
import { KeyedNode } from "./Utils";

interface Props {
  ach: ACHState | null;
  gotoLink: () => void;
  gotoVerify: () => void;
}

export const LinkBanner = (_props: unknown): JSX.Element => (
  <Banner
    color="SocialOrange"
    textStyle={styles.bannerText}
    text={
      <>
        <Text style={[TextStyles.fontWeight600, { fontSize: fontSize + 4 }]}>
          Tgthr Card disabled
        </Text>
        <Text>{`\n`}</Text>
        <Text>
          You and your partner will need to link your bank accounts before your
          Tgthr Cards will work, tap here to start
        </Text>
      </>
    }
  />
);

export const VerifyBanner = (_props: unknown): JSX.Element => (
  <Banner
    color="SocialOrange"
    textStyle={styles.bannerText}
    text={
      <>
        <Text style={[TextStyles.fontWeight600, { fontSize: fontSize + 4 }]}>
          Tgthr Card disabled
        </Text>
        <Text>{`\n`}</Text>
        <Text>
          Tap here to verify your bank account, you'll need to do this before
          your Tgthr Cards will work
        </Text>
      </>
    }
  />
);

export const makeFSNodes = (props: Props): KeyedNode[] => {
  if (props.ach === null) {
    return [];
  }

  if (props.ach.abaExists === false || props.ach.ddaExists === false) {
    return [
      {
        key: "FSLinkNeeded",
        node: (
          <TouchableOpacity style={styles.warningCard} onPress={props.gotoLink}>
            <LinkBanner />
          </TouchableOpacity>
        ),
      },
    ];
  } else if (props.ach.verified !== true) {
    return [
      {
        key: "FSVerifyNeeded",
        node: (
          <TouchableOpacity
            style={styles.warningCard}
            onPress={props.gotoVerify}
          >
            <VerifyBanner />
          </TouchableOpacity>
        ),
      },
    ];
  }

  return [];
};

export const useMakeFSBanner = (): KeyedNode[] => {
  const userstate = useGetUser();
  const history = useHistory();

  const ach = userstate === null ? null : userstate.user.ach;

  const gotoLink = () => {
    void Analytics.track("User FSLinkBanner Tapped", {
      ach: ach,
    });
    history.push(SettingsPaths.fsList);
  };

  const gotoVerify = () => {
    void Analytics.track("User FSVerifyBanner Tapped", {
      ach: ach,
    });
    history.push(SettingsPaths.fsList);
  };

  return makeFSNodes({
    ach: ach,
    gotoLink: gotoLink,
    gotoVerify: gotoVerify,
  });
};
