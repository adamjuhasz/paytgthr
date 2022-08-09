import * as mailcheck from "mailcheck";
import { tlds } from "./ValidTLDs";

export const normalizeEmail = (email: string): string =>
  email.trim().replace(" ", "");

export const validFormat = (email: string): boolean => {
  if (email === "") {
    return false;
  }

  // eslint-disable-next-line no-useless-escape
  const regX = /^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]{1,64}@[a-zA-Z0-9-\.]+\.[a-zA-Z0-9-]+$/;
  if (regX.test(email) === false) {
    return false;
  }

  const domain = email.split(".").slice(-1)[0].toUpperCase();
  if (tlds.find((e) => e === domain) === undefined) {
    return false;
  }

  return true;
};

export const misspelledEmail = (email: string): string | undefined => {
  if (email.length <= 3) {
    return undefined;
  }

  const suggestion = mailcheck.run({
    email,
    secondLevelDomains: ["yahoo", "hotmail", "live", "outlook", "gmail"],
  });
  if (suggestion === undefined) {
    return undefined;
  } else {
    return suggestion.full;
  }
};

export type EmailSuggestion = ReturnType<typeof misspelledEmail>;
