/* eslint-disable @typescript-eslint/no-explicit-any */

export type TrackEvent = (
  event: string,
  properties?:
    | {
        [key: string]: any;
      }
    | undefined
) => Promise<void>;

export interface Analytics {
  middleware: () => void;
  track: TrackEvent;
  screen: (
    name: string,
    properties?:
      | {
          [key: string]: any;
        }
      | undefined
  ) => Promise<void>;
  identify: (
    user: string,
    traits?:
      | {
          [key: string]: any;
        }
      | undefined
  ) => Promise<void>;
  setup: () => Promise<void>;
  reset: () => void;
  disable: () => void;
  setTrait: (trait: string, value: string) => Promise<void>;
}
