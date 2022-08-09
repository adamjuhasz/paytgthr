import React from "react";
import { useSelector } from "react-redux";
import { useQuery } from "react-query";
import { ActivityIndicator } from "react-native";
import { capitalize, defaultTo, split } from "lodash";

import { State as GlobalState } from "../../../../State/State";
import {
  getCurrentUserState,
  queryPath as stateQueryPath,
} from "../../../../Actions/User/GetUserState";
import FundingSourceList, { Account } from "./FundingSourceList";

interface Props {
  goBack: () => void;
  gotoVerify: () => void;
  gotoAddNew: () => void;
}

const FundingSourceListHOC = (props: Props): JSX.Element => {
  const { baseURL } = useSelector((state: GlobalState) => ({
    baseURL: state.baseURL,
  }));
  const stateQuery = useQuery([stateQueryPath], getCurrentUserState(baseURL));

  const normalizer = (str: string): string => {
    const ws = split(str, " ");
    const caps = ws.map(capitalize);
    return caps.join(" ");
  };

  if (stateQuery.data !== undefined) {
    const accountDetails: Account[] =
      stateQuery.data.user.ach.abaExists === false ||
      stateQuery.data.user.ach.ddaExists === false
        ? []
        : [
            {
              accountName: normalizer(
                defaultTo(stateQuery.data.user.ach.accountName, "Bank Account")
              ),
              bankName: normalizer(
                defaultTo(stateQuery.data.user.ach.bankName, "Unknown Bank")
              ),
              verified: defaultTo(stateQuery.data.user.ach.verified, false),
            },
          ];
    return <FundingSourceList {...props} accounts={accountDetails} />;
  } else {
    return <ActivityIndicator />;
  }
};

export default FundingSourceListHOC;
