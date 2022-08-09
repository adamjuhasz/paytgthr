/* global RequestInit, RequestInfo, Response */

import fetch from "../Global/Fetch";
import console from "../Global/Console";
import {
  commonGetHeaders,
  commonOptions,
  commonPostHeaders,
  fetchUA,
} from "../Fetch/Options";
import { Non200Error, UnauthorizedError } from "./Errors/FetchErrors";
export {
  commonGetHeaders,
  commonOptions,
  commonPostHeaders,
  fetchUA,
  Non200Error,
  UnauthorizedError,
};

export class TimeoutError extends Error {
  constructor() {
    super("TimeoutError");
    this.name = "TimeoutError";
  }
}

export function timeout<T>(ms: number): Promise<T> {
  return new Promise<T>(function (_resolve, reject) {
    setTimeout(function () {
      reject(new TimeoutError());
    }, ms);
  });
}

export const OneSecond = 1000;
export const TwoSeconds = 2000;
export const TenSeconds = 10000;

export const retryFetch = async (
  timeoutMs: number,
  url: RequestInfo,
  config: RequestInit,
  retries = 1
): Promise<Response> => {
  try {
    const res = await Promise.race([
      fetch(url, config),
      timeout<Response>(timeoutMs),
    ]);
    return res;
  } catch (err) {
    if (err instanceof TimeoutError && retries > 0) {
      console.log(`${url.toString()} timed out`);
      return retryFetch(timeoutMs, url, config, retries - 1);
    }
    throw err;
  }
};
