import { Link } from "react-router-dom";
import useGetPayment from "./UseGetPayment";
import { currToStr } from "../Shared/Currency";

interface Props {
  payment: string;
}

export default function ShowPayment(props: Props): JSX.Element {
  const payment = useGetPayment(props.payment);

  if (payment.status === "success") {
    return (
      <span>
        {payment.data.payment.type} of {currToStr(payment.data.payment.amount)}{" "}
        <Link
          to={`/payment/${props.payment}`}
          className="text-indigo-500 underline"
        >
          ({props.payment.split("-")[0]})
        </Link>
      </span>
    );
  }

  return (
    <Link
      to={`/payment/${props.payment}`}
      className="text-indigo-500 underline"
    >
      Payment {props.payment.split("-")[0]}
    </Link>
  );
}
