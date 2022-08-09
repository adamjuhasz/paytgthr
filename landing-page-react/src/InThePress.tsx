import { Helmet } from "react-helmet";

import NavBar from "./Shared/Navigation/Nav";
import Footer from "./Shared/Footer";
import FoundingVideo from "./InThePress/FoundingVideo";
import FireSideWithBud from "./InThePress/FireSideWithBud";
import FintechMagazine from "./InThePress/FintechMagazine";

export default function InThePress() {
  return (
    <>
      <Helmet>
        <title>Pay Tgthr in the press</title>
        <meta
          content="Keep tabs on where Pay Tgthr has been in the press"
          name="Description"
        />
      </Helmet>
      <NavBar />

      <FintechMagazine />
      <FireSideWithBud />
      <FoundingVideo />

      <Footer />
    </>
  );
}
