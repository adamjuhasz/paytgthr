import { AlfieThunk } from "../State/Store";

export const markSeen = (seenStatus: boolean): AlfieThunk => async (
  dispatch,
  _getState
) => {
  dispatch({ type: "markseen", status: seenStatus });
  return Promise.resolve(undefined);
};
