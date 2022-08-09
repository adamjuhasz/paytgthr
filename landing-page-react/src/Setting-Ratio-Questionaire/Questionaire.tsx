import { useState } from "react";
import {
  ArrowLeftIcon,
  ArrowRightIcon,
  CalculatorIcon,
} from "@heroicons/react/solid";
import { Helmet } from "react-helmet";

import NavBar from "../Shared/Navigation/Nav";
import Footer from "../Shared/Footer";

import questions from "./questions";

import Intro from "./Intro";
import MultipleChoice from "./MutlipleChoice";
import Steps from "./Steps";
import SinglePlayerResults from "./SinglePlayerResults";
import MultiPlayerResults from "./MultiPlayerResults";

type QuestionID = typeof questions[number]["id"];
type AnswerID = typeof questions[number]["answers"][number]["id"];

export default function Questionaire() {
  const [stepIndex, setStepIdx] = useState(-1);
  const [selectedAnswer, setSelected] = useState<null | AnswerID>(null);
  const [answers, setAnswers] = useState<Partial<Record<QuestionID, AnswerID>>>(
    {}
  );

  console.log(stepIndex);
  const singlePlayer = questions.filter((q) => q.style === "single-player");
  const multiPlayer = questions.filter((q) => q.style === "multi-player");

  const questionBatch =
    stepIndex > singlePlayer.length
      ? [...singlePlayer, ...multiPlayer]
      : singlePlayer;
  const currentStep =
    stepIndex > singlePlayer.length
      ? questionBatch[stepIndex - 1]
      : questionBatch[stepIndex];
  const currentIndex =
    stepIndex > singlePlayer.length ? stepIndex - 1 : stepIndex;

  let content = <></>;

  switch (stepIndex) {
    case -1:
      content = (
        <>
          <Steps stepsLength={questionBatch.length} current={currentIndex} />
          <Intro />
        </>
      );
      break;

    case singlePlayer.length:
      content = (
        <SinglePlayerResults
          answers={answers as Record<QuestionID, AnswerID>}
        />
      );
      break;

    case [...singlePlayer, ...multiPlayer].length + 1:
      content = (
        <MultiPlayerResults answers={answers as Record<QuestionID, AnswerID>} />
      );
      break;

    default:
      content = (
        <>
          <Steps stepsLength={questionBatch.length} current={currentIndex} />
          <MultipleChoice
            question={currentStep.question}
            answers={currentStep.answers}
            setAnswer={setSelected}
            defaultValue={answers[currentStep.id]}
          />
        </>
      );
      break;
  }

  const prevBtnDisabled = stepIndex <= 0;
  let nextBtnDisabled = selectedAnswer === null;
  let nextBtnOnClick = () => {
    if (selectedAnswer === null) {
      return;
    }

    const newAnswers = { ...answers };
    newAnswers[currentStep.id] = selectedAnswer;
    setAnswers(newAnswers);

    setStepIdx(stepIndex + 1);
  };

  if (stepIndex === -1) {
    nextBtnDisabled = false;
    nextBtnOnClick = () => {
      setStepIdx(stepIndex + 1);
    };
  }

  if (stepIndex === singlePlayer.length) {
    nextBtnDisabled = false;
    nextBtnOnClick = () => {
      setStepIdx(stepIndex + 1);
    };
  }

  if (stepIndex === [...singlePlayer, ...multiPlayer].length + 1) {
    nextBtnDisabled = true;
    nextBtnOnClick = () => {
      return;
    };
  }

  return (
    <>
      <Helmet>
        <title>Couple's expense split ratio calculator</title>
        <meta
          content="Calculate the best split for you two as a partner for your shared expenses and purchases"
          name="Description"
        />
      </Helmet>
      <NavBar />
      <div className="max-w-xl mx-auto flex flex-col items-center my-4">
        {content}
        <div className="flex justify-between items-center w-full">
          <button
            disabled={prevBtnDisabled}
            onClick={() => setStepIdx(stepIndex - 1)}
            type="button"
            className={[
              "inline-flex items-center px-6 py-3 border border-transparent shadow-sm text-base font-medium rounded-md text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-rose-500",
              prevBtnDisabled
                ? "bg-rose-400 hover:bg-rose-400 cursor-not-allowed"
                : "bg-rose-600 hover:bg-rose-700",
            ].join(" ")}
          >
            <ArrowLeftIcon className="-ml-1 mr-3 h-5 w-5" aria-hidden="true" />
            Prev
          </button>
          <button
            disabled={nextBtnDisabled}
            onClick={nextBtnOnClick}
            type="button"
            className={[
              "inline-flex items-center px-6 py-3 border border-transparent shadow-sm text-base font-medium rounded-md text-white   focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-rose-500",
              nextBtnDisabled
                ? "bg-rose-400 hover:bg-rose-400 cursor-not-allowed"
                : "bg-rose-600 hover:bg-rose-700",
            ].join(" ")}
          >
            {currentIndex === questionBatch.length - 1 ? "Calculate" : "Next"}
            {currentIndex === questionBatch.length - 1 ? (
              <CalculatorIcon
                className="ml-3 -mr-1 h-5 w-5"
                aria-hidden="true"
              />
            ) : (
              <ArrowRightIcon
                className="ml-3 -mr-1 h-5 w-5"
                aria-hidden="true"
              />
            )}
          </button>
        </div>
      </div>
      <Footer />
    </>
  );
}
