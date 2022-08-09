import React, { ErrorInfo } from "react";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import console from "../../Global/Console";

import ErrorBoundary from "./ErrorBoundary";

interface State {
  hasError: boolean;
  stacktrace: string | null;
}

type Props = React.PropsWithChildren<unknown>;

class ErrorBoundaryHOC extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      hasError: false,
      stacktrace: null,
    };
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  static getDerivedStateFromError(_error: unknown): State {
    // Update state so the next render will show the fallback UI.
    return { hasError: true, stacktrace: null };
  }

  componentDidCatch(error: Error, errorInfo: ErrorInfo): void {
    // You can also log the error to an error reporting service

    console.error("ErrorBoundaryHOC error", error, errorInfo);
    this.setState({ hasError: true, stacktrace: errorInfo.componentStack });
    Analytics.track("Application Crashed", {
      reason: "React",
      stacktrace: errorInfo.componentStack,
      errorName: error.name,
      errorMessage: error.message,
      errorStack: error.stack === undefined ? null : error.stack,
    }).catch((err) => {
      console.error("Error tracking event", err);
    });
  }

  render(): JSX.Element {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return <ErrorBoundary />;
    }

    return <>{this.props.children}</>;
  }
}

export default ErrorBoundaryHOC;
