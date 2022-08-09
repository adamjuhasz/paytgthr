import { Link } from "react-router-dom";

import useGetProgram from "./UseGetProgram";
import { path } from "./EditProgram";

interface Props {
  program: string;
}

export default function ShowProgram(props: Props): JSX.Element {
  const program = useGetProgram(props.program);

  let content = <></>;
  if (program.status !== "success") {
    content = <span>{props.program}</span>;
  } else {
    content = <span>{program.data.refName}</span>;
  }

  return (
    <Link to={path.replace(":pid", props.program)} className="text-indigo-500">
      {content}
    </Link>
  );
}
