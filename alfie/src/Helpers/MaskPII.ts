import { mapValues } from "lodash";
import { validFormat } from "../Helpers/Email";

export const redacted = "<Redacted>"; // could also be "undefined"

export const maskPII = (
  obj: Record<string, unknown>
): Record<string, unknown> =>
  mapValues(obj, (o) => {
    switch (typeof o) {
      case "string":
        //redact email
        if (validFormat(o)) {
          return redacted;
        }

        //redact phone
        if (/^(1)?\d{10}$/.test(o)) {
          return redacted;
        }
        return o;

      default:
        return o;
    }
  });
