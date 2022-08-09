import { PropsWithChildren, ReactNode } from "react";

interface Props {
  header: ReactNode;
  subHeader: ReactNode;
}

export default function DescriptionList(
  props: PropsWithChildren<Props>
): JSX.Element {
  return (
    <div className="m-2">
      <div>
        <h3 className="text-lg leading-6 font-medium text-gray-900">
          {props.header}
        </h3>
        <p className="mt-1 max-w-2xl text-sm text-gray-500">
          {props.subHeader}
        </p>
      </div>
      <div className="mt-5 border-t border-gray-200">
        <dl className="divide-y divide-gray-200">{props.children}</dl>
      </div>
    </div>
  );
}

export const Row = (props: {
  left: ReactNode;
  right: ReactNode;
  clipboard?: string;
  button?: { text: string; onClick: () => void };
}): JSX.Element => (
  <div className="py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4">
    <dt className="text-sm font-medium text-gray-500">
      <div className="flex flex-row justify-between">
        <div>{props.left}</div>
        <div>
          {props.clipboard === undefined ? (
            <></>
          ) : (
            <button
              onClick={() => {
                void navigator.clipboard.writeText(props.clipboard || "");
              }}
              className="font-normal text-xs border-2 border-gray-400 text-gray-400 rounded-md p-0.5"
            >
              Copy
            </button>
          )}
        </div>
      </div>
    </dt>
    <dd className="mt-1 flex text-sm text-gray-900 sm:mt-0 sm:col-span-2 items-center">
      <span className="flex-grow">{props.right}</span>
      {props.button === undefined ? (
        <></>
      ) : (
        <span className="ml-4 flex-shrink-0">
          <button
            onClick={props.button.onClick}
            type="button"
            className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
          >
            {props.button.text}
          </button>
        </span>
      )}
    </dd>
  </div>
);
