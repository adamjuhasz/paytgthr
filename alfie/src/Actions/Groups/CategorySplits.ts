import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

export interface LandingPageCategory {
  id: string;
  enabled: boolean;
  userSplit: number;
}

export interface Category {
  id: string;
  title: string;
  description: string;
  emoji: string;
  enabled: boolean;
  userSplit: number;
}

export const convertFromLPCatToCat = (
  cats: LandingPageCategory[]
): Category[] => {
  const defaultCategories: Category[] = [
    {
      id: "Category000",
      title: "Primary split",
      emoji: "💳",
      enabled: true,
      description: "Most things",
      userSplit: 50,
    },
    {
      id: "Category001",
      title: "Auto and Transportation",
      emoji: "🚕",
      enabled: false,
      description: "Gas Stations, Uber, Lyft, Taxis",
      userSplit: 50,
    },
    {
      id: "Category002",
      title: "Groceries, Big Box Stores",
      emoji: "🥑",
      enabled: false,
      description: "Groceries, Big Box Stores",
      userSplit: 50,
    },
    {
      id: "Category003",
      title: "Media Subscriptions",
      emoji: "📺",
      enabled: false,
      description: "Netflix, Hulu, Disney+",
      userSplit: 50,
    },
    {
      id: "Category004",
      title: "Vets, Pet Food, and Supplies",
      emoji: "🐶",
      enabled: false,
      description: "Pet Food and Supplies",
      userSplit: 50,
    },
    {
      id: "Category005",
      title: "Restaurants, Bars, Takeout",
      emoji: "🍽",
      enabled: false,
      description: "Restaurants, Bars, Takeout",
      userSplit: 50,
    },
    {
      id: "Category006",
      title: "Utilities, Internet, Phone",
      emoji: "⚡️",
      enabled: false,
      description: "Utilities, Internet, Phone",
      userSplit: 50,
    },
  ];

  const catDB: Record<string, LandingPageCategory | undefined> = {};
  cats.forEach((cat) => {
    catDB[cat.id] = cat;
  });

  return defaultCategories.map((cat) => {
    const dbEntry = catDB[cat.id];
    if (dbEntry !== undefined) {
      cat.enabled = dbEntry.enabled;
      cat.userSplit = dbEntry.userSplit;
    }
    return cat;
  });
};

export const path = (groupId: string): string =>
  `/app/group/${groupId}/set/split/categories`;

export const getCategorySplits = (baseURL: string) => async (
  _key: string,
  { groupId }: { groupId: string }
): Promise<Category[]> => {
  const url = `${baseURL}${path(groupId)}`;

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as LandingPageCategory[];
      return convertFromLPCatToCat(body);
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`getCategorySplits had return status of ${res.status}`);
  }
};

export const setCategorySplits = (baseURL: string) => async (input: {
  groupId: string;
  catagories: Category[];
}): Promise<void> => {
  const url = `${baseURL}${path(input.groupId)}`;

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: JSON.stringify(input.catagories),
  });

  switch (res.status) {
    case 200:
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`getCategorySplits had return status of ${res.status}`);
  }
};
