type GroupStatus = "groupactive" | "groupclosed" | "groupdenied";

export interface Group {
  id: string;
  createdat: string;
  end: string | null;
  members: { user: string; accepted: boolean }[];
  revision: number;
  split: {
    ratio: { denominator: number; numerator: number };
    user: string;
    approved: boolean;
  }[];
  start: null;
  status: GroupStatus;
  version: "1.0";
}
