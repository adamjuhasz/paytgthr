/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable no-console */
/* global console */

import { remoteConsole } from "../Actions/Console/SendConsole";

interface ConsoleEntry {
  log: "error" | "log";
  time: Date;
  args: any[];
}
const consoleLog: ConsoleEntry[] = [];

const addToLog = (entry: ConsoleEntry): void => {
  if (consoleLog.length >= 100) {
    consoleLog.shift();
  }
  consoleLog.push(entry);
};

export const getLog = (n = -10): ConsoleEntry[] => {
  return consoleLog.slice(n);
};

const consoleObj = {
  assert: console.assert,
  clear: console.clear,
  count: console.count,
  error: (...args: any[]): void => {
    addToLog({
      log: "error",
      args: args,
      time: new Date(),
    });
    if (__DEV__) {
      return console.error(...args);
    }
    void remoteConsole("error", args);
  },
  group: console.group,
  groupCollapsed: console.groupCollapsed,
  groupEnd: console.groupEnd,
  info: console.info,
  log: (...args: any[]): void => {
    addToLog({
      log: "log",
      args: args,
      time: new Date(),
    });
    if (__DEV__) {
      return console.log(...args);
    }
    void remoteConsole("log", args);
  },
  table: console.table,
  time: console.time,
  timeEnd: console.timeEnd,
  trace: console.trace,
  warn: console.warn,
};

export default consoleObj;
