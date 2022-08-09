import { phoneVerifier } from "../../src/Helpers/Phone";

describe("Phone Verifier", () => {
  it("rejects starting with 1", () => {
    expect(phoneVerifier("123-123-1234")).toBeNull();
  });

  it("accepts good numbers", () => {
    expect(phoneVerifier("345-555-5998")).toEqual("345-555-5998");
  });

  it("rejects short", () => {
    expect(phoneVerifier("345-555")).toBeNull();
  });

  it("rejects non nomarlized", () => {
    expect(phoneVerifier("3455551234")).toBeNull();
  });
});
