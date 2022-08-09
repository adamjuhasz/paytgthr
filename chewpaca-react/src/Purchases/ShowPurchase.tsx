import { Link } from "react-router-dom";

import UseGetPurchase from "./UseGetPurchase";
import { currToStr } from "../Shared/Currency";

interface Props {
  purchase: string;
}

export default function ShowPurchase(props: Props): JSX.Element {
  const purchase = UseGetPurchase(props.purchase);

  if (purchase.status === "success") {
    return (
      <span>
        <Link
          to={`/transaction/${props.purchase}`}
          className="text-indigo-500 underline"
        >
          {purchase.data.purchase.description}
        </Link>{" "}
        {currToStr(purchase.data.purchase.displayamount)}
      </span>
    );
  }

  return <span>{props.purchase.split("-")[0]}</span>;
}
