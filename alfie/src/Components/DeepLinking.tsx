import React, { useEffect } from "react";
import Analytics from "../PlatformSpecific/SegmentAnalytcs";
import { Linking } from "react-native";
import Constants from "expo-constants";
import console from "../Global/Console";
import { useHistory } from "../PlatformSpecific/react-router";

let singletonLock = false;

export const DeepLinking = (): JSX.Element => {
  const history = useHistory();

  useEffect(() => {
    const handler = async ({ url }: { url: string }) => {
      if (blockedLink(url)) {
        return; /* bug with Expo? */
      }

      await Analytics.track("DeepLink Opened", {
        url: url,
        path: normalizeLink(url),
      });
      history.push(normalizeLink(url));
    };

    let branchremove = () => {
      return;
    };

    try {
      if (Constants.appOwnership === "standalone") {
        void (async () => {
          const Branch = await import("expo-branch");

          branchremove = Branch.default.subscribe((event) => {
            if (event.error !== undefined && event.error !== null) {
              console.error(`Error from Branch: ${event.error}`);
              return;
            }

            const { params, uri } = event;

            // params will never be null if error is null
            console.log("branch deeplink", params, uri);
            if (params["+non_branch_link"] !== undefined) {
              void handler({ url: params["+non_branch_link"] });
              return;
            }

            if (params["$deeplink_path"] !== undefined) {
              void handler({ url: params["$deeplink_path"] });
              return;
            }

            if (params["+clicked_branch_link"] === false) {
              return;
            }

            console.log("Error: unknown branch link", event);
          });
        })();
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
      console.error("expo-branch subscribe error", e.toString());
    }

    Linking.addEventListener("url", handler);

    return () => {
      console.log("DeepLinking unmounting");
      Linking.removeEventListener("url", handler);
      branchremove();
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  useEffect(() => {
    if (singletonLock === false) {
      singletonLock = true;

      void Linking.getInitialURL().then((incomingLink) => {
        if (incomingLink !== null) {
          if (blockedLink(incomingLink)) {
            console.log(
              `DeepLinking getInitialURL was ${incomingLink} and got blocked`
            );
            return; /* bug with Expo? */
          }

          void Analytics.track("DeepLink Initial", {
            url: incomingLink,
            path: normalizeLink(incomingLink),
          });

          history.replace(normalizeLink(incomingLink));
        }
      });
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return <></>;
};

export default DeepLinking;

const normalizeLink = (url: string): string => {
  return url
    .replace("https://paytgthr.com", "")
    .replace("http://paytgthr.com", "")
    .replace("https://paytgthr.dev", "")
    .replace("http://paytgthr.dev", "");
};

const blockedLink = (url: string): boolean => {
  if (url.startsWith("exp://")) {
    return true;
  }

  if (url.startsWith("https://expo.io")) {
    return true;
  }

  if (url.startsWith("https://exp.host")) {
    return true;
  }

  if (url.startsWith("https://link.paytgthr.com")) {
    return true;
  }

  if (url.startsWith("https://tgthr-alternate.app.link")) {
    return true;
  }

  switch (url) {
    case "exp://exp.host/@ajuhasz/alfie":
      return true;

    case "https://expo.io:443/@ajuhasz/alfie":
      return true;
  }

  return false;
};
