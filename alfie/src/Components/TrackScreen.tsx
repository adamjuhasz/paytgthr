import React, { useEffect } from "react";
import Analytics from "../PlatformSpecific/SegmentAnalytcs";
import { useLocation } from "../PlatformSpecific/react-router";
import console from "../Global/Console";

interface Props {
  screen: string;
}

export const TrackScreen = ({ screen }: Props): JSX.Element => {
  const { pathname } = useLocation();
  useEffect(() => {
    Analytics.screen(screen, {
      locationPath: pathname,
    }).catch((err) => {
      console.error("Error tracking screen", err);
    });
  }, [screen, pathname]);

  return <></>;
};

export default TrackScreen;
