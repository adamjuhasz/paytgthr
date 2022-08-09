import React, {
  PropsWithChildren,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import ToastDisplay from "./ToastDisplay";
import { debounce } from "lodash";

interface ToastInterface {
  showToast: (toast: string) => void;
}

export const ToastContext = React.createContext<ToastInterface>({
  showToast: (_str: string) => {
    return;
  },
});

type ToasTextUpdater = React.Dispatch<React.SetStateAction<ToastString | null>>;

export const ToastProvider = (
  props: PropsWithChildren<unknown>
): JSX.Element => {
  const setText = useRef<ToasTextUpdater | null>(null);

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const clearText = useMemo(
    () =>
      debounce(
        () => {
          if (setText.current === null) {
            return;
          }
          setText.current(null);
        },
        2000,
        { trailing: true }
      ),
    [setText]
  );

  const displayText = useMemo(
    () => (str: string) => {
      if (setText.current === null) {
        return;
      }

      setText.current({ text: str, id: (Math.random() + 1).toString(36) });
      clearText();
    },
    [setText, clearText]
  );

  return (
    <>
      <ToastContext.Provider value={{ showToast: displayText }}>
        {props.children}
      </ToastContext.Provider>
      <ToastShower t={setText} />
    </>
  );
};

interface ToastString {
  text: string;
  id: string;
}

const ToastShower = ({
  t,
}: {
  t: React.MutableRefObject<ToasTextUpdater | null>;
}): JSX.Element => {
  const [toastText, setToastText] = useState<ToastString | null>(null);

  useEffect(() => {
    t.current = setToastText;
    return () => {
      t.current === null;
    };
  }, [t, setToastText]);

  if (toastText === null) {
    return <></>;
  }

  return <ToastDisplay toastStr={toastText.text} toastId={toastText.id} />;
};

export default ToastContext;
