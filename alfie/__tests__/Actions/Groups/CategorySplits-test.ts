import { convertFromLPCatToCat } from "../../../src/Actions/Groups/CategorySplits";

describe("convertFromLPCatToCat", () => {
  it("injects 1 downloaded cat", () => {
    const res = convertFromLPCatToCat([
      {
        id: "Category001",
        enabled: true,
        userSplit: 90,
      },
    ]);

    expect(res[1]).not.toEqual({
      id: "Category001",
      title: "Auto and Transportation",
      emoji: "🚕",
      enabled: false,
      description: "Gas Stations, Uber, Lyft, Taxis",
      userSplit: 50,
    });

    expect(res[1]).toEqual({
      id: "Category001",
      title: "Auto and Transportation",
      emoji: "🚕",
      enabled: true,
      description: "Gas Stations, Uber, Lyft, Taxis",
      userSplit: 90,
    });
  });

  it("injects 2 downloaded cats", () => {
    const res = convertFromLPCatToCat([
      {
        id: "Category003",
        enabled: false,
        userSplit: 10,
      },
      {
        id: "Category001",
        enabled: true,
        userSplit: 90,
      },
    ]);

    expect(res[1]).not.toEqual({
      id: "Category001",
      title: "Auto and Transportation",
      emoji: "🚕",
      enabled: false,
      description: "Gas Stations, Uber, Lyft, Taxis",
      userSplit: 50,
    });

    expect(res[1]).toEqual({
      id: "Category001",
      title: "Auto and Transportation",
      emoji: "🚕",
      enabled: true,
      description: "Gas Stations, Uber, Lyft, Taxis",
      userSplit: 90,
    });

    expect(res[3]).not.toEqual({
      id: "Category003",
      title: "Media Subscriptions",
      emoji: "📺",
      enabled: false,
      description: "Netflix, Hulu, Disney+",
      userSplit: 50,
    });

    expect(res[3]).toEqual({
      id: "Category003",
      title: "Media Subscriptions",
      emoji: "📺",
      enabled: false,
      description: "Netflix, Hulu, Disney+",
      userSplit: 10,
    });
  });
});
