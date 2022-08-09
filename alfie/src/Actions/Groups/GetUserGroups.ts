import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";
import { GroupStates } from "../../Types/Group";

export interface GroupMembership {
  accepted: boolean;
  user: string;
}

export interface GroupMembershipWithUser extends GroupMembership {
  fname: string;
  email: string;
  lname: string;
  id: string;
}

export interface GroupSplit {
  approved: boolean;
  ratio: { denominator: number; numerator: number };
  user: string;
}

export interface GroupSplitWithUser extends GroupSplit {
  fname: string;
  email: string;
  lname: string;
  id: string;
}

export interface GroupModel {
  id: string;
  revision: string;
  status: GroupStates;
  version: string;
  start: null | string;
  end: null | string;
  members: GroupMembership[];
  split: GroupSplit[];
}

export interface GroupModelWithUser extends GroupModel {
  members: GroupMembershipWithUser[];
  split: GroupSplitWithUser[];
}

interface UserMiniModel {
  email: string;
  fname: string;
  id: string;
  lname: string;
}

interface GroupResponse {
  groups: GroupModel[];
  users: UserMiniModel[];
  version: 1;
}

export const queryPath = "/app/user/groups";

export const getUsersGroups = (baseURL: string) => async (): Promise<
  GroupModelWithUser[]
> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body: GroupResponse = await res.json();

      const userDB: { [user: string]: UserMiniModel } = {};
      body.users.forEach((user) => {
        userDB[user.id] = user;
      });

      const groupsExtended: GroupModelWithUser[] = body.groups.map((group) => {
        const g: GroupModelWithUser = {
          ...group,
          members: group.members.map((member) => {
            const theUser = userDB[member.user];
            const memberExt: GroupMembershipWithUser = {
              ...member,
              ...theUser,
            };
            return memberExt;
          }),
          split: group.split.map((split) => {
            const theUser = userDB[split.user];
            const splitExt: GroupSplitWithUser = { ...split, ...theUser };
            return splitExt;
          }),
        };
        return g;
      });

      return groupsExtended;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`getUsersGroups had return status of ${res.status}`);
  }
};
