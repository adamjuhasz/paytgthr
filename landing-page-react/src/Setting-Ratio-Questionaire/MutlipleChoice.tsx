import { useEffect, useState, Dispatch, SetStateAction, SVGProps } from "react";
import { RadioGroup } from "@headlessui/react";

import questions from "./questions";

type AnswerID = typeof questions[number]["answers"][number]["id"];

interface Props {
  question: string;
  answers: typeof questions[number]["answers"];
  setAnswer: Dispatch<SetStateAction<AnswerID | null>>;
  defaultValue?: AnswerID;
}

export default function MultipleChoice(props: Props) {
  const [selected, setSelected] = useState<null | Props["answers"][number]>(
    null
  );

  useEffect(() => {
    const answers: { id: AnswerID; text: string }[] = props.answers as any;

    const defaultSel: typeof selected =
      props.defaultValue === undefined
        ? null
        : (answers.filter(
            (a) => a.id === props.defaultValue
          )[0] as typeof selected);

    setSelected(defaultSel);
  }, [props.defaultValue, props.question, props.answers]);

  useEffect(() => {
    props.setAnswer(selected === null ? null : selected.id);
  }, [selected, props]);

  return (
    <div className="w-full px-4 py-16">
      <div className="prose prose-red prose-lg mb-4">
        <p>{props.question}</p>
      </div>
      <div className="w-full max-w-md mx-auto">
        <RadioGroup value={selected} onChange={setSelected}>
          <RadioGroup.Label className="sr-only">Server size</RadioGroup.Label>
          <div className="space-y-2">
            {props.answers.map((answer) => (
              <RadioGroup.Option
                key={answer.text}
                value={answer}
                className={({ active, checked }) =>
                  [
                    active
                      ? "active ring-2 ring-offset-2 ring-offset-rose-300 ring-white ring-opacity-60"
                      : "",
                    checked
                      ? "checked bg-rose-600 bg-opacity-100 text-white"
                      : "bg-white",
                    "relative rounded-lg shadow-md px-5 py-4 cursor-pointer flex focus:outline-none",
                  ].join(" ")
                }
              >
                {({ active, checked }) => (
                  <>
                    <div className="flex items-center justify-between w-full">
                      <div className="flex items-center">
                        <div className="text-sm">
                          <RadioGroup.Label
                            as="p"
                            className={`font-medium  ${
                              checked ? "text-white " : "text-gray-900"
                            }`}
                          >
                            {answer.text}
                          </RadioGroup.Label>
                          <RadioGroup.Description
                            as="span"
                            className={`inline ${
                              checked ? "text-rose-100" : "text-gray-500"
                            }`}
                          ></RadioGroup.Description>
                        </div>
                      </div>
                      {checked && (
                        <div className="flex-shrink-0 text-white mx-1">
                          <CheckIcon className="w-6 h-6" />
                        </div>
                      )}
                    </div>
                  </>
                )}
              </RadioGroup.Option>
            ))}
          </div>
        </RadioGroup>
      </div>
    </div>
  );
}

function CheckIcon(props: SVGProps<SVGSVGElement>) {
  return (
    <svg viewBox="0 0 24 24" fill="none" {...props}>
      <circle cx={12} cy={12} r={12} fill="#fff" opacity="0.2" />
      <path
        d="M7 13l3 3 7-7"
        stroke="#fff"
        strokeWidth={1.5}
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </svg>
  );
}
