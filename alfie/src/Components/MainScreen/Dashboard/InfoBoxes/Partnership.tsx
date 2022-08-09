import React from "react";
import { Text, TouchableOpacity } from "react-native";
import { useHistory } from "../../../../PlatformSpecific/react-router";
import { defaultTo } from "lodash";
import Analytics from "../../../../PlatformSpecific/SegmentAnalytcs";

import Banner, { fontSize } from "../../../Banner/Banner";
import TextStyles from "../../../Styling/Text";
import styles from "./Styles";
import { KeyedNode } from "./Utils";
import useGetGroups from "../../../Hooks/UseGetGroups";
import SettingsPaths from "../../../Routers/SettingsRouter/Paths";

export type PartnerStatus = "NeedsInvite" | "Active" | "!Loading";

const InvitePartnerBanner = (_props: unknown) => (
  <Banner
    color="SocialRed"
    textStyle={styles.bannerText}
    text={
      <>
        <Text style={[TextStyles.fontWeight600, { fontSize: fontSize + 4 }]}>
          Tgthr Card disabled
        </Text>
        <Text>{`\n`}</Text>
        <Text>
          Tap here to invite a partner to start splitting purchases today
        </Text>
      </>
    }
  />
);

export const makePartnershipNodes = (
  partnerStatus: PartnerStatus,
  gotoGroup: () => void
): KeyedNode[] => {
  switch (partnerStatus) {
    case "NeedsInvite":
      return [
        {
          key: "NeedsInvite",
          node: (
            <TouchableOpacity style={styles.warningCard} onPress={gotoGroup}>
              <InvitePartnerBanner />
            </TouchableOpacity>
          ),
        },
      ];
      break;

    case "Active":
    case "!Loading":
      return [];
  }
};

export const useMakePartnershipBanner = (): KeyedNode[] => {
  const history = useHistory();
  const groups = useGetGroups();

  const activeGroups = defaultTo(groups.data, []).filter(
    (group) => group.status === "groupactive"
  );

  let partnerStatus: PartnerStatus = "!Loading";
  if (groups.status !== "success") {
    partnerStatus = "!Loading";
  } else if (activeGroups.length >= 1) {
    partnerStatus = "Active";
  } else {
    partnerStatus = "NeedsInvite";
  }

  const gotoGroup = () => {
    void Analytics.track("User PartnerBanner Tapped", {
      partnerStatus: partnerStatus,
    });
    history.push(SettingsPaths.viewInviteCode);
  };

  return makePartnershipNodes(partnerStatus, gotoGroup);
};
