import { Link } from "react-router-dom";
import useGetBoost from "./UseGetBoost";
import { path } from "./BoostDetail";

interface Props {
  boostid: string;
}

export default function BoostLink(props: Props): JSX.Element {
  const boost = useGetBoost(props.boostid);
  return (
    <Link
      to={path.replace(":rewardid", props.boostid)}
      className="text-indigo-500 no-underline"
    >
      {boost.status === "success" ? (
        <span>
          {boost.data.boostName} ({boost.data.boostId.split("-")[0]})
        </span>
      ) : (
        <abbr title={props.boostid} className="no-underline">
          {props.boostid.split("-")[0]}
        </abbr>
      )}
    </Link>
  );
}
