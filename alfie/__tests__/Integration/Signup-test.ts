/**
 * @group integration
 */
import { randomBytes } from "crypto";

import { isLoginError, signup } from "../../src/Actions/Login";
import { getName, submitName } from "../../src/Actions/SignUp/SignUpName";
import { submitDisclosure } from "../../src/Actions/SignUp/SignUpDisclosures";
import { submitLegalConsent } from "../../src/Actions/SignUp/signUpLegalConsent";
import {
  getAddress,
  submitAddress,
} from "../../src/Actions/SignUp/SignUpAddress";
import { getCurrentUserState } from "../../src/Actions/User/GetUserState";
import { submitManualDetails } from "../../src/Actions/SignUp/BankAccount/SignUpManualLink";
import { submitAmount } from "../../src/Actions/SignUp/BankAccount/SignUpVerifyManual";

import { State } from "../../src/State/State";

const baseURL = "https://paytgthr.dev";

xdescribe("sign up", () => {
  it("rejects short password", async () => {
    const dispatch = () => undefined;
    const getState = () =>
      ({
        baseURL,
      } as State);

    const email = `${randomBytes(16).toString("hex")}@paytgthr.test`;
    const result = await signup(email, "short")(dispatch, getState).catch(
      (e: unknown) => {
        if (isLoginError(e)) {
          return e;
        }
        throw e;
      }
    );

    expect(result).toEqual({
      emailError: "None",
      passwordError: "PasswordRule",
    });
  });

  it("rejects bad email", async () => {
    const dispatch = () => undefined;
    const getState = () =>
      ({
        baseURL: "https://paytgthr.dev",
      } as State);

    const email = `${randomBytes(16).toString("hex")}@paytgthr`;
    const result = await signup(email, "password")(dispatch, getState).catch(
      (e: unknown) => {
        if (isLoginError(e)) {
          return e;
        }
        throw e;
      }
    );

    expect(result).toEqual({
      emailError: "EmailFormat",
      passwordError: "None",
    });
  });

  it("creates account", async () => {
    const dispatch = () => undefined;
    const getState = () =>
      ({
        baseURL: "https://paytgthr.dev",
      } as State);

    const email = `${randomBytes(16).toString("hex")}@paytgthr.test`;
    const result = await signup(email, "password")(dispatch, getState);

    expect(result).toBeUndefined();
  });

  it("gets no name", async () => {
    const getter = getName(baseURL)();
    await expect(getter).resolves.toBeDefined();
    const user = await getter;
    expect(user.user.firstName).toBeNull();
    expect(user.user.lastName).toBeNull();
  });

  it("sets name", async () => {
    const setter = submitName(baseURL)("John", "Waz");
    await expect(setter).resolves.toBeUndefined();
  });

  it("gets new name", async () => {
    const getter = getName(baseURL)();
    await expect(getter).resolves.toBeDefined();
    const user = await getter;
    expect(user.user.firstName).toEqual("John");
    expect(user.user.lastName).toEqual("Waz");
  });

  it("sets disclosure", async () => {
    const promise = submitDisclosure(baseURL);
    await expect(promise).resolves.toBeUndefined();
  });

  it("sets consent", async () => {
    const promise = submitLegalConsent(baseURL)();
    await expect(promise).resolves.toBeUndefined();
  });

  it("gets address that's null", async () => {
    const promise = getAddress(baseURL)();
    await expect(promise).resolves.toBeDefined();

    const address = await promise;
    expect(address.street).toBe("");
    expect(address.apt).toBe("");
    expect(address.city).toBe("");
    expect(address.state).toBe("");
    expect(address.zip).toBe("");
  });

  it("sets address", async () => {
    const promise = submitAddress(baseURL)({
      street: "123 main st",
      apt: "apt 5",
      city: "San Fran",
      state: "CA",
      zip: "12345",
    });
    await expect(promise).resolves.toBeUndefined();
  });

  it("gets address that's filled", async () => {
    const promise = getAddress(baseURL)();
    await expect(promise).resolves.toBeDefined();

    const address = await promise;
    expect(address.street).toBe("123 main st");
    expect(address.apt).toBe("apt 5");
    expect(address.city).toBe("San Fran");
    expect(address.state).toBe("CA");
    expect(address.zip).toBe("12345");
  });

  it("get user state thats all filled", async () => {
    const promise = getCurrentUserState(baseURL)();
    await expect(promise).resolves.toBeDefined();

    const user = await promise;
    expect(["UserActive", "UserWaitingOnKYC"]).toContain(user.user.status.tag);
  });

  it("sets manual FS links", async () => {
    const userStateBefore = await getCurrentUserState(baseURL)();
    expect(userStateBefore.user.ach.ddaExists).toBe(false);

    const promise = submitManualDetails(baseURL)(
      "1234567",
      "101002716",
      "Checking",
      "B of A"
    );

    await expect(promise).resolves.toBeUndefined();

    const userStateAfter = await getCurrentUserState(baseURL)();
    expect(userStateAfter.user.ach.ddaExists).toBe(true);
    expect(userStateAfter.user.ach.verified).toBe(false);
  });

  it("verifies manual FS", async () => {
    const endCondition = 100;
    for (let i = 1; i <= endCondition; i += 1) {
      const promise = submitAmount(baseURL)(`${i}`);
      try {
        await promise;
        break;
      } catch (e) {
        if (i >= endCondition) {
          throw e;
        }
        continue;
      }
    }

    const userStateAfter = await getCurrentUserState(baseURL)();
    expect(userStateAfter.user.ach.verified).toBe(true);
  }, 30000);
});
