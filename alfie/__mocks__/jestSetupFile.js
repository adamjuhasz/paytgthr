/* eslint-disable */
import mockAsyncStorage from "@react-native-async-storage/async-storage/jest/async-storage-mock";

jest.mock("@react-native-async-storage/async-storage", () => mockAsyncStorage);

global.FormData = require("form-data");

const fetch = require("node-fetch");
global.fetch = fetch;

global.__JEST__ = true;
