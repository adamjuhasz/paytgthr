import { useSelector } from "react-redux";
import { useQuery } from "react-query";

import {
  queryPath as cardQueryPath,
  getCards,
} from "../../Actions/Card/GetCards";
import { EnhancedCardModel } from "../../Actions/Card/Types";
import { State as GlobalState } from "../../State/State";

export const path = cardQueryPath;
const staleTimeMs = 30 * 1000; // 60 sec

type Return =
  | { success: true; data: EnhancedCardModel[] }
  | { success: false; data: undefined };

export default function useGetCards(): Return {
  const { baseURL } = useSelector((state: GlobalState) => ({
    baseURL: state.baseURL,
  }));

  const cardQuery = useQuery([cardQueryPath], getCards(baseURL), {
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  if (cardQuery.isSuccess === true && cardQuery.data !== undefined) {
    return { success: true, data: cardQuery.data };
  } else {
    return { success: false, data: undefined };
  }
}
