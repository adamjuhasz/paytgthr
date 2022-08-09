import React, { useEffect } from "react";
import { useHistory, useLocation } from "../../PlatformSpecific/react-router";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import { defaultTo } from "lodash";
import console from "../../Global/Console";

import NoRouteMatch from "./NoRouteMatch";

interface Props {
  router: string;
}

const NoRouteMatchHOC = (
  props: React.PropsWithChildren<Props>
): JSX.Element => {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const location = useLocation<Record<string, any>>();
  const history = useHistory();

  useEffect(() => {
    const locationState = defaultTo(location.state, {});
    void Analytics.track("Error NoRouteMatch", {
      location: location.pathname,
      router: props.router,
      ...locationState,
    });
    console.error(
      `Could not route to location "${location.pathname}" with router ${props.router}`,
      location
    );
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <NoRouteMatch action={() => history.push("/app")}>
      {props.children}
    </NoRouteMatch>
  );
};

export default NoRouteMatchHOC;
