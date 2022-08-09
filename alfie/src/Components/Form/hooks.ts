import React, { useRef, useState } from "react";
import {
  NativeSyntheticEvent,
  TextInput,
  TextInputSubmitEditingEventData,
} from "react-native";

type UseText = (
  initialValue?: string
) => [
  string,
  React.Dispatch<React.SetStateAction<string>>,
  React.RefObject<TextInput>,
  (evt: NativeSyntheticEvent<TextInputSubmitEditingEventData>) => void
];
export const useTextInput: UseText = (initialValue = "") => {
  const [text, setText] = useState<string>(initialValue);
  const textRef = useRef<TextInput>(null);
  const focus = (
    _evt: NativeSyntheticEvent<TextInputSubmitEditingEventData>
  ) => {
    if (textRef.current !== null) {
      textRef.current.focus();
    }
  };

  return [text, setText, textRef, focus];
};
