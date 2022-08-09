import React, { useContext } from "react";
import { ActivityIndicator } from "react-native";
import { useMutation, useQuery, useQueryCache } from "react-query";
import { useSelector } from "react-redux";
import { useParams } from "../../../PlatformSpecific/react-router";

import { State } from "../../../State/State";
import {
  getUsersGroups,
  queryPath as groupQueryPath,
} from "../../../Actions/Groups/GetUserGroups";
import {
  Category,
  getCategorySplits,
  path,
  setCategorySplits,
} from "../../../Actions/Groups/CategorySplits";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import FromScreen, { Navigation } from "../../Form/FormScreen";
import { ThemeContext } from "../../Theming/ThemeContext";

import GroupSplit from "./GroupSplit";

interface Props {
  navigation: Navigation;
  goBack: () => void;
  screenIndex: number;
  screenCount: number;
}

const GroupSplitHOC = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const { baseURL, myUID } = useSelector((state: State) => ({
    baseURL: state.baseURL,
    myUID: state.userInfo.userId,
  }));

  const { gid } = useParams<{
    gid: string;
  }>();

  const catsQuery = useQuery(
    [path(gid), { groupId: gid }],
    getCategorySplits(baseURL)
  );

  const groupQuery = useQuery([groupQueryPath], getUsersGroups(baseURL), {
    staleTime: 10000,
    enabled: myUID !== null,
  });

  const [mutate, { isLoading: mutateLoading, isSuccess: mutateSuccess }] =
    useMutation(setCategorySplits(baseURL));

  const cache = useQueryCache();

  const mutateCatSplits = async (cats: Category[]) => {
    if (gid === "00000000-0000-0000-0000-000000000000") {
      props.goBack();
      return;
    }

    await mutate({ groupId: gid, catagories: cats });
    await cache.invalidateQueries([path(gid), { groupId: gid }]);
    props.goBack();
  };

  if (catsQuery.data === undefined || mutateLoading || mutateSuccess) {
    return (
      <FromScreen
        title="Purchase splitting"
        buttons={<></>}
        navigation={props.navigation}
      >
        <ActivityIndicator size="large" color={theme.textColor} />
      </FromScreen>
    );
  } else {
    let partnersName: string | undefined;
    if (groupQuery.data !== undefined) {
      groupQuery.data.forEach((group) => {
        if (group.id === gid) {
          group.members.forEach((member) => {
            if (member.id !== myUID) {
              if (typeof member.fname === "string") {
                partnersName = member.fname.slice(0, 1);
              } else {
                partnersName = "P";
              }
            }
          });
        }
      });
    }

    return (
      <GroupSplit
        categoryList={catsQuery.data}
        navigation={props.navigation}
        submit={mutateCatSplits}
        partnerInitial={partnersName}
        trackEvent={Analytics.track}
        screenIndex={props.screenIndex}
        screenCount={props.screenCount}
      />
    );
  }
};

export default GroupSplitHOC;
