/* eslint-disable @typescript-eslint/require-await */
/* global window */
import console from "../Global/Console";

export default {
  getItem: async (key: string): Promise<string | null> => {
    console.log(`Loading Redux store from ${key}`);
    return window.localStorage.getItem(key);
  },
  setItem: async (key: string, value: string): Promise<void> => {
    return window.localStorage.setItem(key, value);
  },
};
