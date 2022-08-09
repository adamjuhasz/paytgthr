import useGetScoreInfo from "./UseGetScoreInfo";
import { currToStr } from "../Shared/Currency";

interface Props {
  score: number;
}

export default function ShowDollarToNext(props: Props): JSX.Element {
  const scoreInfo = useGetScoreInfo(props.score);

  if (scoreInfo.status === "success") {
    return <span>{currToStr(scoreInfo.data.spendToNext)}</span>;
  }

  return <span></span>;
}
