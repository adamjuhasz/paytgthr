import { Currency } from "./Types";

export const currToStr = (currency: Currency): string =>
  (currency[1] / currency[2]).toLocaleString("en-US", {
    style: "currency",
    currency: currency[0],
  });
