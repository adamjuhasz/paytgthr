import useGetScoreInfo from "./UseGetScoreInfo";
import { currToStr } from "../Shared/Currency";

interface Props {
  score: number;
}

export default function ShowTrustScore(props: Props): JSX.Element {
  const scoreInfo = useGetScoreInfo(props.score);

  if (scoreInfo.status === "success") {
    return <span>{currToStr(scoreInfo.data.limit)}</span>;
  }

  return <span>{props.score}</span>;
}
