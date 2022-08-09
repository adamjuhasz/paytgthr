import { Data } from "../../../SectionedList/Types";

export interface KeyedNode {
  key: string;
  node: JSX.Element;
}

export const wrapNodes = (data: KeyedNode[]): Data[] =>
  data.map((d) => ({ ...d, type: "node" }));

export const unKeyNode = (data: KeyedNode[]): JSX.Element | undefined => {
  if (data.length === 0) {
    return undefined;
  } else {
    return data[0].node;
  }
};
