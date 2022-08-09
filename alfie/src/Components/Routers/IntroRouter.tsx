import React, { useEffect } from "react";
import {
  Redirect,
  Route,
  Switch,
  useHistory,
} from "../../PlatformSpecific/react-router";
import { useDispatch } from "react-redux";

import { AlfieDispatch } from "../../State/Store";
import { markSeen } from "../../Actions/IntroSeen";

import TrackScreen from "../TrackScreen";
import AutoSplit from "../IntroV2/AutoSplit";
import NotChecking from "../IntroV2/NotChecking";
import GrowingLimit from "../IntroV2/GrowingLimit";
import Pricing from "../IntroV2/Pricing";

export const paths = {
  start: "/intro",
  page1: "/intro/1",
  page2: "/intro/2",
  page3: "/intro/3",
  page4: "/intro/4",
};

interface SeenProps {
  as: boolean;
}

const SeenIntro = ({ as }: SeenProps) => {
  const dispatch = useDispatch<AlfieDispatch>();

  useEffect(() => {
    void dispatch(markSeen(as));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);
  return <></>;
};

const IntroRouter = (): JSX.Element => {
  const history = useHistory();

  return (
    <Switch>
      <Route exact path={paths.start}>
        <Redirect to={paths.page1} />
      </Route>
      <Route exact path={paths.page1}>
        <SeenIntro as={true} />
        <TrackScreen screen="Intro" />
        <AutoSplit
          pageIndex={0}
          pageCount={4}
          next={() => history.push(paths.page2)}
        />
      </Route>
      <Route exact path={paths.page2}>
        <TrackScreen screen="Intro2" />
        <NotChecking
          pageIndex={1}
          pageCount={4}
          next={() => history.push(paths.page3)}
        />
      </Route>
      <Route exact path={paths.page3}>
        <TrackScreen screen="Intro3" />
        <GrowingLimit
          pageIndex={2}
          pageCount={4}
          next={() => history.push(paths.page4)}
        />
      </Route>
      <Route exact path={paths.page4}>
        <TrackScreen screen="Intro4" />
        <Pricing
          pageIndex={3}
          pageCount={4}
          next={() => history.push("/app")}
        />
      </Route>
    </Switch>
  );
};

export default IntroRouter;
