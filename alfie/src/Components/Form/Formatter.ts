import React from "react";

export const formatInput = (
  firstIndex: number,
  secondIndex: number,
  seperator: string
) => (value: string): string => {
  const origVal = value;
  const val = origVal.replace(/\D/g, "");
  const first = val.slice(0, firstIndex);
  const second = val.slice(firstIndex, secondIndex);
  const third = val.slice(secondIndex);

  let newVal = first;
  if (first.length === firstIndex && second.length > 0) {
    newVal = newVal.concat(seperator);
  }
  newVal = newVal.concat(second);
  if (second.length === secondIndex - firstIndex && third.length > 0) {
    newVal = newVal.concat(seperator);
  }
  newVal = newVal.concat(third);

  return newVal;
};

export const limitInput = (
  fn: React.Dispatch<React.SetStateAction<string>>,
  limit: number
) => (t: string): void => {
  if (t.replace(/(\D)/g, "").length <= limit) {
    // count only digits, replace non-digits with blank
    fn(t);
  }
};
