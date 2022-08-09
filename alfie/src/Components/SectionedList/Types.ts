import { IconProps } from "../Icons/Types";

export type Section = SectionText | SectionNone;

export interface SectionText {
  sectionType: "text";
  title: string;
}

export interface SectionNone {
  sectionType: "none";
  key: string;
}

export interface TextData {
  type: "text";
  text: string;
  helpText?: string;
  action?: () => void;
  icon?: (p: IconProps) => JSX.Element;
  progress?: { value: number; color: string };
  key?: string;
}

export interface NodeData {
  type: "node";
  node: JSX.Element;
  key: string;
}

export type Data = TextData | NodeData;
