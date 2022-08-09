import { AlfieThunk } from "../State/Store";

type Cluster = "DEV" | "PROD";

export const setCluster = (cluster: Cluster): AlfieThunk => async (
  dispatch,
  _getState
) => {
  let newURL = "https://paytgthr.com";
  switch (cluster) {
    case "DEV":
      newURL = "https://paytgthr.dev";
      break;

    case "PROD":
      newURL = "https://paytgthr.com";
      break;
  }

  dispatch({ type: "setcluster", baseURL: newURL });
  return Promise.resolve(undefined);
};
