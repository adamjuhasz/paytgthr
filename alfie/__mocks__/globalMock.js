/* eslint-disable */

jest.mock("global", () => ({
  ...global,
  WebSocket: function WebSocket() {},
}));
