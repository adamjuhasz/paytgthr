import React, { useRef } from "react";
import { WebView } from "react-native-webview";
import {
  ShouldStartLoadRequest,
  WebViewErrorEvent,
} from "react-native-webview/lib/WebViewTypes";
import {
  LinkErrorCode,
  LinkErrorType,
  LinkEvent,
  LinkEventName,
  LinkExit,
  LinkExitMetadataStatus,
  LinkSuccess,
} from "./types";
import queryString from "query-string";

interface PlaidLinkProps {
  linkToken: string;
  onEvent?(event: LinkEvent): void;
  onExit?(exit: LinkExit): void;
  onSuccess?(success: LinkSuccess): void;
  onError?(evt: WebViewErrorEvent): void;
}

export default function PlaidLink({
  linkToken,
  onEvent,
  onExit,
  onSuccess,
  onError,
}: PlaidLinkProps): JSX.Element {
  const webviewRef = useRef<WebView>(null);

  const handleNavigationStateChange = (event: ShouldStartLoadRequest) => {
    if (event.url.startsWith("plaidlink://")) {
      const eventParams = queryString.parse(event.url.replace(/.*\?/, ""));

      const linkSessionId = eventParams.link_session_id as string;
      const mfaType = eventParams.mfa_type as string;
      const requestId = eventParams.request_id as string;
      const viewName = eventParams.view_name as string;
      const errorCode = eventParams.error_code as string;
      const errorMessage = eventParams.error_message as string;
      const errorType = eventParams.error_type as string;
      const exitStatus = eventParams.exist_status as string;
      const institutionId = eventParams.institution_id as string;
      const institutionName = eventParams.institution_name as string;
      const institutionSearchQuery = eventParams.institution_search_query as string;
      const timestamp = eventParams.timestamp as string;

      if (event.url.startsWith("plaidlink://event") && onEvent) {
        onEvent({
          eventName: eventParams.event_name as LinkEventName,
          metadata: {
            linkSessionId,
            mfaType,
            requestId,
            viewName,
            errorCode,
            errorMessage,
            errorType,
            exitStatus,
            institutionId,
            institutionName,
            institutionSearchQuery,
            timestamp,
          },
        });
      } else if (event.url.startsWith("plaidlink://exit") && onExit) {
        onExit({
          error: {
            errorCode: LinkErrorCode[errorCode as keyof typeof LinkErrorCode],
            errorMessage: eventParams.error_message as string,
            errorType: LinkErrorType[errorType as keyof typeof LinkErrorType],
          },
          metadata: {
            status:
              LinkExitMetadataStatus[
                exitStatus as keyof typeof LinkExitMetadataStatus
              ],
            institution: {
              id: institutionId,
              name: institutionName,
            },
            linkSessionId,
            requestId,
          },
        });
      } else if (event.url.startsWith("plaidlink://connected") && onSuccess) {
        const publicToken = eventParams.public_token as string;
        const accounts = JSON.parse(eventParams.accounts as string);
        onSuccess({
          publicToken,
          metadata: {
            institution: {
              id: institutionId,
              name: institutionName,
            },
            accounts,
            linkSessionId,
          },
        });
      }
      return false;
    }
    return true;
  };

  return (
    <WebView
      source={{
        uri: `https://cdn.plaid.com/link/v2/stable/link.html?isWebview=true&token=${linkToken}`,
      }}
      ref={webviewRef}
      onError={(evt: WebViewErrorEvent) => {
        if (onError !== undefined) {
          onError(evt);
        }
      }}
      originWhitelist={["https://*", "plaidlink://*"]}
      onShouldStartLoadWithRequest={handleNavigationStateChange}
    />
  );
}
