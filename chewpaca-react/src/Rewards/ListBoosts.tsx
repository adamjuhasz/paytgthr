import useGetBoosts from "./UseGetBoosts";

import BoostList from "./BoostList";

export const path = "/rewards/list";

export default function ListBoosts(): JSX.Element {
  const boosts = useGetBoosts();
  if (boosts.status === "success") {
    return (
      <div className="p-2">
        <BoostList boosts={boosts.data} />
      </div>
    );
  }
  return <div>Loading...</div>;
}
