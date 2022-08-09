import React, { Component } from "react";
import { Animated, Dimensions, StyleSheet, ViewStyle } from "react-native";

const windowHeight = Dimensions.get("window").height;
const windowWidth = Dimensions.get("window").width;

interface Props {
  index: number;
  onAnimationComplete: Animated.EndCallback | undefined;
  colors: string[];
  duration: number;
  size: number;
  bsize: number;
}

class ConfettiBit extends Component<Props> {
  _yAnimation: Animated.Value;
  color: string;
  left: number;
  _rotateAnimation: Animated.AnimatedInterpolation;
  _xAnimation: Animated.AnimatedInterpolation;

  constructor(props: Props) {
    super(props);
    this._yAnimation = new Animated.Value(this.randomValue(-20, -10));
    this.color = this.randomColor(this.props.colors);
    this.left = this.randomValue(0, windowWidth);
    const rotationOutput = `${this.randomValue(-220, 220)}deg`;
    const startRotation = `${this.randomValue(-220, 220)}deg`;
    this._rotateAnimation = this._yAnimation.interpolate({
      inputRange: [0, windowHeight / 2, windowHeight],
      outputRange: [startRotation, rotationOutput, rotationOutput],
    });

    const xDistance = this.randomIntValue(
      (windowWidth / 3) * -1,
      windowWidth / 3
    );
    this._xAnimation = this._yAnimation.interpolate({
      inputRange: [0, windowHeight],
      outputRange: [0, xDistance],
    });
  }

  componentDidMount(): void {
    const { duration } = this.props;
    Animated.timing(this._yAnimation, {
      duration: duration + this.randomIntValue(duration * 0.2, duration * -0.2),
      toValue: windowHeight + 1.25,
      useNativeDriver: true,
    }).start(this.props.onAnimationComplete);
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  getTransformStyle(): any {
    return {
      transform: [
        { translateY: this._yAnimation },
        { translateX: this._xAnimation },
        { rotate: this._rotateAnimation },
      ],
    };
  }

  getConfettiStyle(): ViewStyle {
    const { index, size, bsize } = this.props;
    const bigConfetti = {
      height: 5.5 * size,
      width: 11 * size,
      borderBottomLeftRadius: 5 * bsize,
      borderBottomRightRadius: 5 * bsize,
      borderTopLeftRadius: 2.6 * bsize,
      borderTopRightRadius: 2.6 * bsize,
    };
    const smallConfetti = {
      height: 4.5 * size,
      width: 8 * size,
      borderBottomLeftRadius: 2.5 * bsize,
      borderBottomRightRadius: 2.5 * bsize,
      borderTopLeftRadius: 1.3 * bsize,
      borderTopRightRadius: 1.3 * bsize,
    };
    return index % 5 === 0 ? smallConfetti : bigConfetti;
  }

  randomValue(min: number, max: number): number {
    return Math.random() * (max - min) + min;
  }

  randomIntValue(min: number, max: number): number {
    return Math.floor(Math.random() * (max - min) + min);
  }

  randomColor<T>(colors: T[]): T {
    return colors[this.randomIntValue(0, colors.length)];
  }

  render(): JSX.Element {
    const { ...otherProps } = this.props;
    return (
      <Animated.View
        style={[
          styles.confetti,
          this.getConfettiStyle(),
          this.getTransformStyle(),
          { marginLeft: this.left, backgroundColor: this.color },
        ]}
        {...otherProps}
      />
    );
  }
}

const styles = StyleSheet.create({
  confetti: {
    marginTop: 0,
    position: "absolute",
  },
});

export default ConfettiBit;
