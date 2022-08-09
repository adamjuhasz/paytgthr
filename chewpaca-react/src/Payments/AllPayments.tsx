import useGetAllPayments from "./UseGetAllPayments";
import PaymentTable from "./PaymentTable";

export const path = "/payments";

export default function AllPaymentds(_props: unknown): JSX.Element {
  const paymentQuery = useGetAllPayments();

  if (paymentQuery.status !== "success") {
    return <div>Loading ({paymentQuery.status})</div>;
  }

  return (
    <div className="m-2">
      <PaymentTable payments={paymentQuery.data} />
    </div>
  );
}
