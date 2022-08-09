import {
  misspelledEmail,
  validFormat,
  normalizeEmail,
} from "../../src/Helpers/Email";

describe("Correct misspelled email", () => {
  it("corrects gmial.com", () => {
    expect(misspelledEmail("karronsmith07@gmial.com")).toEqual(
      "karronsmith07@gmail.com"
    );
  });

  it("corrects gnail.com", () => {
    expect(misspelledEmail("land22326@gnail.com")).toEqual(
      "land22326@gmail.com"
    );
  });

  it("leaves gmail.com alone", () => {
    expect(misspelledEmail("land22326@gmail.com")).toBeUndefined();
  });

  it("ignores short emails", () => {
    expect(misspelledEmail("lan")).toBeUndefined();
  });
});

describe("detects invalid email", () => {
  it("accepts good email", () => {
    expect(validFormat("karronsmith07@gmail.com")).toEqual(true);
  });

  it("accepts standard email", () => {
    expect(validFormat("email@example.com")).toEqual(true);
  });

  it("accepts standard email with period", () => {
    expect(validFormat("firstname.lastname@example.com")).toEqual(true);
  });

  it("accepts with subdomain", () => {
    expect(validFormat("email@subdomain.example.com")).toEqual(true);
  });

  it("accepts with +", () => {
    expect(validFormat("efirstname+lastname@example.com")).toEqual(true);
  });

  xit("accepts with double quotes", () => {
    expect(validFormat('"email"@example.com')).toEqual(true);
  });

  it("accepts with number email", () => {
    expect(validFormat("1234567890@example.com")).toEqual(true);
  });

  it("accepts with dash", () => {
    expect(validFormat("email@example-one.com")).toEqual(true);
  });

  it("accepts with underscores", () => {
    expect(validFormat("_______@example.com")).toEqual(true);
  });

  it("accepts with modern TLD", () => {
    expect(validFormat("email@example.museum")).toEqual(true);
  });

  it("accepts with small subdomain", () => {
    expect(validFormat("email@example.co.jp")).toEqual(true);
  });

  it("accepts with dash in mailbox", () => {
    expect(validFormat("firstname-lastname@example.com")).toEqual(true);
  });

  xit("WERID 1", () => {
    expect(validFormat("much.”more unusual”@example.com")).toEqual(true);
  });

  xit("WERID 2", () => {
    expect(validFormat("very.unusual.”@”.unusual.com@example.com")).toEqual(
      true
    );
  });

  xit("WERID 3", () => {
    expect(
      validFormat(
        `very.”(),:;<>[]”.VERY.”very@\\ "very”.unusual@strange.example.com`
      )
    ).toEqual(true);
  });

  it("rejects .con due to TLD", () => {
    expect(validFormat("karronsmith07@gmail.con")).toEqual(false);
  });

  it("rejects with no @", () => {
    expect(validFormat("Abc.example.com")).toEqual(false);
  });

  it("rejects with too many @", () => {
    expect(validFormat("A@b@c@example.com")).toEqual(false);
  });

  it("rejects special characters", () => {
    expect(validFormat('a"b(c)d,e:f;g<h>i[j\\k]l@example.com')).toEqual(false);
  });

  it("rejects with bad quoting", () => {
    expect(validFormat('just"not"right@example.com')).toEqual(false);
  });

  it("rejects with empty space", () => {
    expect(validFormat("this isnotallowed@example.com")).toEqual(false);
  });

  it("rejects underscores in domain", () => {
    expect(
      validFormat(
        "i_like_underscore@but_its_not_allow_in _this_part.example.com"
      )
    ).toEqual(false);
  });

  it("rejects long local", () => {
    expect(
      validFormat(
        "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
      )
    ).toEqual(false);
  });

  xit("rejects on double periods in ..", () => {
    expect(validFormat("me@example..com")).toEqual(false);
  });

  it("no domain info", () => {
    expect(validFormat("plainaddress")).toEqual(false);
  });

  it("no alpha", () => {
    expect(validFormat("#@%^%#$@#$@#.com")).toEqual(false);
  });

  it("no mailbox", () => {
    expect(validFormat("@example.com")).toEqual(false);
  });

  it("has name too", () => {
    expect(validFormat("Joe Smith <email@example.com>")).toEqual(false);
  });

  it("just domains", () => {
    expect(validFormat("email.example.com")).toEqual(false);
  });

  it("double domains", () => {
    expect(validFormat("email@example@example.com")).toEqual(false);
  });

  xit("start with period", () => {
    expect(validFormat(".email@example.com")).toEqual(false);
  });

  xit("end with period", () => {
    expect(validFormat("email.@example.com")).toEqual(false);
  });

  xit("double period", () => {
    expect(validFormat("email..email@example.com")).toEqual(false);
  });

  it("name at end", () => {
    expect(validFormat("email@example.com (Joe Smith)")).toEqual(false);
  });

  it("no TLD", () => {
    expect(validFormat("email@example")).toEqual(false);
  });

  xit("start with hyphen", () => {
    expect(validFormat("email@-example.com")).toEqual(false);
  });

  xit("domain can't be 'example'", () => {
    expect(validFormat("email@example.web")).toEqual(false);
  });

  it("rejects empty", () => {
    expect(validFormat("")).toEqual(false);
  });
});

describe("normalizer", () => {
  it("strips", () => {
    expect(normalizeEmail(" adAm@ akiTrAM.com  ")).toEqual("adAm@akiTrAM.com");
  });
});
