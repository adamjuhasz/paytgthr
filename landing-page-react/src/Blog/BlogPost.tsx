import { useEffect } from "react";
import smartquotes from "smartquotes";

import NavBar from "../Shared/Navigation/Nav";
import Footer from "../Shared/Footer";

export default function Blog(
  props: React.PropsWithChildren<unknown>
): JSX.Element {
  useEffect(() => {
    smartquotes().listen();
  }, []);

  return (
    <>
      <div className="bg-white">
        <NavBar />
        <main>{props.children}</main>
        <Footer />
      </div>
    </>
  );
}
