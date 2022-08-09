/* eslint-disable react-native/no-inline-styles */
import React from "react";

import FormScreen from "../Form/FormScreen";
import SectionedList from "../SectionedList/SectionedList";
import RightChevron from "../Icons/RightChevron";

interface Props {
  chooseManual: () => void;
  choosePlaid: () => void;
  goBack: () => void;
}

export const LinkTypeChooser = (props: Props): JSX.Element => {
  return (
    <FormScreen
      testID="LinkTypeChooser FormScreen"
      title="Link bank account"
      buttons={<></>}
      navigation={{ type: "action", action: props.goBack }}
      disableInputFeatures={true}
    >
      <SectionedList
        headerStyle={{ marginBottom: 0 }}
        itemStyle={{ marginBottom: 0 }}
        data={[
          {
            sectionType: "text",
            title: "Manual Link",
            data: [
              {
                type: "text",
                text: "",
                helpText: `Enter your account and routing number and we’ll make a small withdrawal (under $2.50) to verify the account is yours. We’ll return the same amount the next day.`,
              },
              {
                type: "text",
                text: "",
                helpText: `Takes 3 - 4 days, your account must have at least a $75 balance.`,
              },
              {
                type: "text",
                text: "Use manual link",
                icon: RightChevron,
                action: props.chooseManual,
              },
            ],
          },
          {
            sectionType: "text",
            title: "Automatic Link",
            data: [
              {
                type: "text",
                text: "",
                helpText: `Start shopping Tgthr today by linking your bank account using Plaid. Note that not all banks support Plaid and your account must have at least a $75 balance.`,
              },
              {
                type: "text",
                text: "",
                helpText: `We’ll make a small withdrawal (under $2.50) to verify we can withdraw money from your account, this will be returned the next day.`,
              },
              {
                type: "text",
                text: "Use auto link",
                icon: RightChevron,
                action: props.choosePlaid,
              },
            ],
          },
        ]}
      />
    </FormScreen>
  );
};

export default LinkTypeChooser;
