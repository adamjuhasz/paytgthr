/* This example requires Tailwind CSS v2.0+ */
import { CheckIcon } from "@heroicons/react/solid";

interface Props {
  stepsLength: number;
  current: number;
}

export default function Steps(props: Props) {
  const steps = new Array(props.stepsLength).fill(null);

  return (
    <nav aria-label="Progress">
      <ol className="flex items-center">
        {steps.map((step, stepIdx) => (
          <li
            key={stepIdx}
            className={[
              stepIdx !== steps.length - 1 ? "pr-8 sm:pr-20" : "",
              "relative",
            ].join(" ")}
          >
            {stepIdx < props.current ? (
              <>
                <div
                  className="absolute inset-0 flex items-center"
                  aria-hidden="true"
                >
                  <div className="h-0.5 w-full bg-rose-600" />
                </div>
                <button className="relative w-8 h-8 flex items-center justify-center bg-rose-600 rounded-full hover:bg-rose-900 cursor-default">
                  <CheckIcon
                    className="w-5 h-5 text-white"
                    aria-hidden="true"
                  />
                </button>
              </>
            ) : stepIdx === props.current ? (
              <>
                <div
                  className="absolute inset-0 flex items-center"
                  aria-hidden="true"
                >
                  <div className="h-0.5 w-full bg-gray-200" />
                </div>
                <button
                  className="relative w-8 h-8 flex items-center justify-center bg-white border-2 border-rose-600 rounded-full cursor-default"
                  aria-current="step"
                >
                  <span
                    className="h-2.5 w-2.5 bg-rose-600 rounded-full"
                    aria-hidden="true"
                  />
                </button>
              </>
            ) : (
              <>
                <div
                  className="absolute inset-0 flex items-center"
                  aria-hidden="true"
                >
                  <div className="h-0.5 w-full bg-gray-200" />
                </div>
                <button className="group relative w-8 h-8 flex items-center justify-center bg-white border-2 border-gray-300 rounded-full hover:border-gray-400 cursor-default">
                  <span
                    className="h-2.5 w-2.5 bg-transparent rounded-full group-hover:bg-gray-300"
                    aria-hidden="true"
                  />
                </button>
              </>
            )}
          </li>
        ))}
      </ol>
    </nav>
  );
}
