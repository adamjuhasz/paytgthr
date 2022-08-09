import GroupList from "../Group/GroupList";

import useGetGroups from "./UseGetGroups";

interface Props {
  user: string;
}

export default function UserGroups({ user }: Props): JSX.Element {
  const groups = useGetGroups(user);

  return groups.status === "success" ? (
    <GroupList groups={groups.data} user={user} />
  ) : (
    <div>loading groups</div>
  );
}
