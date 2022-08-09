import { Helmet } from "react-helmet";

import NavBar from "./Shared/Navigation/Nav";
import Footer from "./Shared/Footer";
import Content from "./Referral/Content";
import FAQ from "./Referral/FAQ";

export const path = "/web/referral";

export default function Referral(): JSX.Element {
  return (
    <>
      <Helmet>
        <title>Pay Tgthr's Referral Program</title>
        <meta
          content="Earn some dough for referring your friend to get a Tgthr Card"
          name="Description"
        />
      </Helmet>
      <NavBar />

      <Content />
      <FAQ />

      <Footer />
    </>
  );
}
