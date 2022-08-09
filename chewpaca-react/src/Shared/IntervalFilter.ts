import { sub } from "date-fns";

export type TimeInterval = "30 days" | "60 days" | "90 days" | "All";

type Keyed<T extends string> = {
  [key in T]: string;
};

export const filterDate = <T extends string>(
  e: Keyed<T>,
  k: T,
  interval: TimeInterval
): boolean => {
  let maxDate = new Date();

  switch (interval) {
    case "30 days":
      maxDate = sub(maxDate, { days: 30 });
      break;

    case "60 days":
      maxDate = sub(maxDate, { days: 60 });
      break;

    case "90 days":
      maxDate = sub(maxDate, { days: 90 });
      break;

    case "All":
    default:
      maxDate = new Date(2018, 1, 1);
      break;
  }

  return new Date(e[k]).valueOf() > maxDate.valueOf();
};
