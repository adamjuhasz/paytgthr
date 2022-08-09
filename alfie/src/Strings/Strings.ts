import { EnStrings } from "./En";

// use English as the template
export type LanguageStrings = typeof EnStrings;
export type Languags = "En";

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export const useStrings = () => {
  const translated: LanguageStrings = EnStrings;
  return { t: translated };
};
