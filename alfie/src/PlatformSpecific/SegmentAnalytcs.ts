/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
/* eslint-disable @typescript-eslint/require-await */
/* global console */

import { Analytics } from "./SegmentAnalyticsType";

const analytics: Analytics = {
  middleware: function () {
    return this;
  },
  track: async function (event: string, properties?: unknown) {
    console.log("Track:", event, properties);
    return;
  },
  screen: async function (_name: string, _properties?: unknown) {
    return;
  },
  identify: async function (_user: string, _traits?: unknown) {
    return;
  },
  setup: async function () {
    return;
  },
  reset: function () {
    return;
  },
  disable: function () {
    return;
  },
  setTrait: async function () {
    return;
  },
};
export default analytics;
