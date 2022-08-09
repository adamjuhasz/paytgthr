/* eslint-disable @typescript-eslint/no-unsafe-member-access */

import React, { useEffect, useRef, useState } from "react";
import console from "../Global/Console";

export interface CancelledError {
  isCanceled: boolean;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/explicit-module-boundary-types
export function wasCancelled(x: any): x is CancelledError {
  return x.isCanceled !== undefined;
}

export interface Cancellable<T> {
  promise: Promise<T>;
  cancel: () => void;
}

export const makeCancelable = <T>(promise: Promise<T>): Cancellable<T> => {
  let hasCanceled_ = false;

  const wrappedPromise = new Promise<T>((resolve, reject) => {
    promise.then(
      (val) => (hasCanceled_ ? reject({ isCanceled: true }) : resolve(val)),
      (error) => (hasCanceled_ ? reject({ isCanceled: true }) : reject(error))
    );
  });

  return {
    promise: wrappedPromise,
    cancel() {
      hasCanceled_ = true;
    },
  };
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const wrapRef = async <T = any>(
  setProgress: React.Dispatch<React.SetStateAction<boolean>>,
  submitted: React.MutableRefObject<Cancellable<T> | null>,
  promise: Promise<T>
): Promise<T> => {
  setProgress(true);
  submitted.current = makeCancelable(promise);
  try {
    const res = await submitted.current.promise;
    setProgress(false);
    return res;
  } catch (err) {
    if (wasCancelled(err)) {
      //do nothing
    } else {
      setProgress(false);
    }
    throw err;
  }
};

export const cancelEffect = (
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  submitted: React.MutableRefObject<Cancellable<any> | null>
): React.EffectCallback => () => () => {
  if (submitted.current !== null) {
    submitted.current.cancel();
  }
};

export const useAsync = <R, E>(
  initialErrors: E
): [boolean, (promise: Promise<R>) => Promise<R>, E] => {
  const [inProgress, setInProgress] = useState(false);
  const [currentErrors, setErrors] = useState<E>(initialErrors);
  const submitted = useRef<Cancellable<R> | null>(null);

  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(cancelEffect(submitted), []);

  const submit = async (promise: Promise<R>) => {
    setInProgress(true);
    submitted.current = makeCancelable(promise);
    try {
      const res = await submitted.current.promise;
      setInProgress(false);
      return res;
    } catch (err) {
      if (wasCancelled(err)) {
        //do nothing
      } else {
        setInProgress(false);
      }
      if (err instanceof Error) {
        console.error("useAsync Error", err);
      } else {
        setErrors(err as E);
      }
      throw err;
    }
  };

  return [inProgress, submit, currentErrors];
};
