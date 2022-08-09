import { dobVerifier } from "../../src/Helpers/DOB";

describe("Finds an incorrect date of birth", () => {
  it("rejects short", () => {
    expect(dobVerifier("10/03")).toBeNull();
  });

  it("rejects non normalized", () => {
    expect(dobVerifier("10-03-1985")).toBeNull();
  });

  xit("rejects non-numbers", () => {
    expect(dobVerifier("aa/bb/ccxx")).toBeNull();
    expect(dobVerifier("aa/11/1985")).toBeNull();
    expect(dobVerifier("01/bb/1985")).toBeNull();
    expect(dobVerifier("01/12/cccc")).toBeNull();
  });

  it("rejects an invalid month", () => {
    expect(dobVerifier("13/01/1985")).toBeNull();
    expect(dobVerifier("00/01/1985")).toBeNull();

    expect(dobVerifier("01/02/1985")).not.toBeNull();
    expect(dobVerifier("12/30/1985")).not.toBeNull();
  });

  it("rejects an invalid day", () => {
    expect(dobVerifier("10/00/1985")).toBeNull();
    expect(dobVerifier("10/32/1985")).toBeNull();
    expect(dobVerifier("02/31/1985")).toBeNull();

    expect(dobVerifier("12/02/1985")).not.toBeNull();
    expect(dobVerifier("12/30/1985")).not.toBeNull();
  });

  it("rejects a 2 digit year", () => {
    expect(dobVerifier("10/03/85")).toBeNull();
  });

  it("rejects more than a 100 years old", () => {
    expect(dobVerifier("10/03/1921")).toBeNull();
  });

  it("rejects less than 18", () => {
    const today = new Date();
    const tooYoung = new Date();
    tooYoung.setFullYear(today.getFullYear() - 18);

    const tooYoungText = `${tooYoung.getMonth() + 1}/${
      tooYoung.getDate() + 1
    }/${tooYoung.getFullYear()}`;

    expect(dobVerifier(tooYoungText)).toBeNull();
  });

  it("accepts 19", () => {
    const today = new Date();
    const tooYoung = new Date();
    tooYoung.setFullYear(today.getFullYear() - 19);

    const tooYoungText = `${
      tooYoung.getMonth() + 1
    }/${tooYoung.getDate()}/${tooYoung.getFullYear()}`;

    expect(dobVerifier(tooYoungText)).not.toBeNull();
  });

  it("accepts middle age", () => {
    expect(dobVerifier("08/02/1985")).toEqual("08/02/1985");
  });
});
