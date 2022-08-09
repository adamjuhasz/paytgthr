import { Route, Routes } from "react-router-dom";

import AllPrograms, { path as allProgramsPath } from "./AllPrograms";
import EditProgram, { path as editProgramPath } from "./EditProgram";
import CreateNewProgram, {
  path as createNewProgramPath,
} from "./CreateNewProgram";
import SpecificProgress, { path as progressPath } from "./SpecificProgress";
import AllProgress, { path as allProgressPath } from "./AllProgress";
import AllCodes, { path as allCodesPath } from "./AllCodes";

export default function LedgerRouter(): JSX.Element {
  return (
    <Routes>
      <Route
        path={allProgramsPath.replace("/referrals", "/")}
        element={<AllPrograms />}
      />
      <Route
        path={editProgramPath.replace("/referrals", "/")}
        element={<EditProgram />}
      />
      <Route
        path={createNewProgramPath.replace("/referrals", "/")}
        element={<CreateNewProgram />}
      />
      <Route
        path={progressPath.replace("/referrals", "/")}
        element={<SpecificProgress />}
      />
      <Route
        path={allProgressPath.replace("/referrals", "/")}
        element={<AllProgress />}
      />
      <Route
        path={allCodesPath.replace("/referrals", "/")}
        element={<AllCodes />}
      />
    </Routes>
  );
}
