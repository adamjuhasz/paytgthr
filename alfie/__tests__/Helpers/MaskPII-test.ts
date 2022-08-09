import { maskPII, redacted } from "../../src/Helpers/MaskPII";

describe("maskPII in objectd", () => {
  it("redacts emails", () => {
    expect(
      maskPII({ identifier: "adam@example.com", id: "123" })
    ).toStrictEqual({
      id: "123",
      identifier: redacted,
    });
  });

  it("keeps bad emails", () => {
    expect(maskPII({ identifier: "adam@example.", id: "123" })).toStrictEqual({
      id: "123",
      identifier: "adam@example.",
    });
  });

  it("redact phone with no (+1)", () => {
    expect(maskPII({ identifier: "7867640987", id: "123" })).toStrictEqual({
      id: "123",
      identifier: redacted,
    });
  });

  it("redact phone with (+1)", () => {
    expect(maskPII({ identifier: "13349879934", id: "123" })).toStrictEqual({
      id: "123",
      identifier: redacted,
    });
  });

  it("keeps random number", () => {
    expect(maskPII({ identifier: "349879934", id: "123" })).toStrictEqual({
      id: "123",
      identifier: "349879934",
    });

    expect(maskPII({ identifier: "34987993444", id: "123" })).toStrictEqual({
      id: "123",
      identifier: "34987993444",
    });
  });
});
