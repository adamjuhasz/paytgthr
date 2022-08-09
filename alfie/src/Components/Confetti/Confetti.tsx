import React from "react";
import { ViewStyle } from "react-native";
import { StyleSheet, View } from "react-native";

import ConfettiBit from "./ConfettiBit";

export interface Props {
  confettiCount?: number;
  timeout?: number;
  startOnLoad?: boolean;
  untilStopped?: boolean;
  duration?: number;
  colors?: string[];
  size?: number;
  bsize?: number;
  style?: ViewStyle;
}

interface State {
  confettis: { key: number }[];
  onComplete: (() => void) | null;
}

class ConfettiView extends React.Component<Props, State> {
  shouldStop: boolean;
  confettiIndex: number;

  state: State = { confettis: [], onComplete: null };

  constructor(props: Props) {
    super(props);
    this.confettiIndex = 0;
    this.shouldStop = false;
  }

  componentDidMount(): void {
    const { startOnLoad = false } = this.props;
    if (startOnLoad) {
      this.startConfetti();
    }
  }

  componentWillUnmount(): void {
    this.stopConfetti();
  }

  startConfetti(onComplete?: () => void): void {
    const { confettis } = this.state;
    const {
      confettiCount = 100,
      timeout = 30,
      untilStopped = false,
    } = this.props;
    this.shouldStop = false;
    if (untilStopped || this.confettiIndex < confettiCount) {
      setTimeout(() => {
        if (this.shouldStop) {
          return;
        } else {
          confettis.push({ key: this.confettiIndex });
          this.confettiIndex++;
          onComplete && this.setState({ onComplete });
          this.setState({ confettis });
          this.startConfetti();
        }
      }, timeout);
    }
  }

  removeConfetti(key: number): void {
    const { confettis, onComplete } = this.state;
    const { confettiCount = 100 } = this.props;
    const index = confettis.findIndex((confetti) => {
      return confetti.key === key;
    });
    confettis.splice(index, 1);
    this.setState({ confettis });
    if (key === confettiCount - 1) {
      this.confettiIndex = 0;
    }
    if (
      confettis.length === 0 &&
      onComplete &&
      typeof onComplete === "function"
    ) {
      onComplete();
    }
  }

  stopConfetti(): void {
    this.shouldStop = true;
    this.confettiIndex = 0;
    const { onComplete } = this.state;
    if (onComplete && typeof onComplete === "function") {
      onComplete();
    }
    this.setState({ confettis: [], onComplete: null });
  }

  render(): JSX.Element {
    const { confettis } = this.state;
    const {
      duration = 6000,
      colors = [
        "rgb(242.2, 102, 68.8)",
        "rgb(255, 198.9, 91.8)",
        "rgb(122.4, 198.9, 163.2)",
        "rgb(76.5, 193.8, 216.7)",
        "rgb(147.9, 99.4, 140.2)",
      ],
      size = 1,
      bsize = 1,
    } = this.props;
    return (
      <View
        style={[StyleSheet.absoluteFill, this.props.style]}
        pointerEvents="none"
      >
        {confettis.map((confetti) => {
          return (
            <ConfettiBit
              key={confetti.key}
              index={confetti.key}
              onAnimationComplete={this.removeConfetti.bind(this, confetti.key)}
              colors={colors}
              duration={duration}
              size={size}
              bsize={bsize}
            />
          );
        })}
      </View>
    );
  }
}

export default ConfettiView;
