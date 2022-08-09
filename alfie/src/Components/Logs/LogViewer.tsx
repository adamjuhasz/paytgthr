import React from "react";
import { SafeAreaView, SectionListData, StyleSheet } from "react-native";

import SectionedList from "../SectionedList/SectionedList";
import { Data, Section } from "../SectionedList/Types";
import { getLog } from "../../Global/Console";

interface Props {
  goBack: () => void;
}

const LogViewer = (props: Props): JSX.Element => {
  const data: SectionListData<Data, Section>[] = [
    {
      sectionType: "none",
      key: "goback",
      data: [{ type: "text", text: "← Go back", action: props.goBack }],
    },
    {
      sectionType: "text",
      title: "Logs",
      data: getLog(-20)
        .reverse()
        .map((l) => ({
          type: "text",
          text: `${l.time.toLocaleString()} "${l.log.toUpperCase()}"`,
          helpText: JSON.stringify(l.args, null, 2),
        })),
    },
  ];
  return (
    <SafeAreaView style={[StyleSheet.absoluteFill]}>
      <SectionedList data={data} />
    </SafeAreaView>
  );
};

export default LogViewer;
