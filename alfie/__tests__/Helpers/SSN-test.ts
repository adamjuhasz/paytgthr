/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable no-undef */
/* eslint-disable @typescript-eslint/no-unsafe-call */
import { ssnVerifier } from "../../src/Helpers/SSN";

describe("SSN format verifier", () => {
  it("ITIN numbers begin with 9, we accept these", () => {
    expect(ssnVerifier("912-34-3345")).toBe("912-34-3345");
  });

  it('SSA will not issue SSNs beginning with the number "666" in positions 1 – 3.', () => {
    expect(ssnVerifier("666-34-3345")).toBeNull();
  });

  it('SSA will not issue SSNs beginning with the number "000" in positions 1 – 3', () => {
    expect(ssnVerifier("000-34-3345")).toBeNull();
  });

  it('SSA will not issue SSNs with the number "00" in positions 4 – 5.', () => {
    expect(ssnVerifier("123-00-3345")).toBeNull();
  });

  it('SSA will not issue SSNs with the number "0000" in positions 6 – 9.', () => {
    expect(ssnVerifier("123-34-0000")).toBeNull();
  });

  it("Allows specific testing one through", () => {
    expect(ssnVerifier("123120000")).toBe("123120000");
  });

  it("Checks length", () => {
    expect(ssnVerifier("12312000")).toBeNull();
  });

  it("Allows good ones through", () => {
    expect(ssnVerifier("123-12-1234")).toBe("123-12-1234");
    expect(ssnVerifier("123121234")).toBe("123121234");
  });
});
