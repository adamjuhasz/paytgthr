import { chain, defaultTo, isFunction, isMatch } from "lodash";
import axios from "axios";

import questions from "./questions";

export type Answer = "50:50" | "60:40" | "70:30";

export type Selection = typeof questions[number]["answers"][number]["id"];
export type Answers = Record<typeof questions[number]["id"], Selection>;

const calculator =
  (deviceId: string) =>
  (answers: Answers): Answer => {
    const counts = chain(answers)
      .values()
      .filter(
        (a) =>
          ["one-pays", "half-half", "half-turns", "inequal"].indexOf(a) !== -1
      )
      .map((a) => (a === "half-turns" ? "half-half" : a))
      .countBy()
      .map((val, key) => ({ id: key, count: val }))
      .orderBy(["count"], ["desc"])
      .value();
    // returns something like [{id: "half-half": 2}, {id: "one-pays": 1}]

    //use rent as the answer
    let rentBasedAnswer: Answer = "50:50";
    switch (answers.rent) {
      case "rent-half":
        rentBasedAnswer = "50:50";
        break;
      case "rent-inequal-small":
        rentBasedAnswer = "60:40";
        break;
      case "rent-inequal-medium":
        rentBasedAnswer = "60:40";
        break;
      case "rent-inequal-large":
        rentBasedAnswer = "70:30";
        break;
    }

    //turn "half-turns" into "half-half" to make pattern matching easier
    const normalizedAnswers = chain(answers)
      .mapValues((i) => (i === "half-turns" ? "half-half" : i))
      .value();

    const huristicAnswer = new PatternMatch<Answers, Answer>(normalizedAnswers)
      .when(
        {
          "dining-out": "one-pays", // this seems like just a chivalry thing
          "food-delivery": "half-half",
          rent: "rent-half",
        },
        "50:50"
      )
      .when({ "dining-out": "inequal", "food-delivery": "inequal" }, "60:40")
      .when({ "utility-bills": "inequal", "food-delivery": "inequal" }, "60:40")
      .when({ "utility-bills": "inequal", "dining-out": "inequal" }, "60:40")
      .answer();

    console.log(
      "calculator (counts, rentBasedAnswer, huristicAnswer)",
      counts,
      rentBasedAnswer,
      huristicAnswer
    );

    let finalAnswer = defaultTo(huristicAnswer, rentBasedAnswer);

    //if user picked all of one split, spit that back out to them
    if (counts.length === 1) {
      switch (counts[0].id) {
        case "one-pays":
          finalAnswer = "70:30";
          break;

        case "half-half":
          finalAnswer = "50:50";
          break;
      }
    }

    void axios.post("/record/event/web", {
      name: `User CalculateSplit`,
      properties: {
        counts: counts,
        rentBasedAnswer: rentBasedAnswer,
        huristicAnswer: huristicAnswer,
        answers: answers,
        finalAnswer: finalAnswer,
      },
      userId: null,
      deviceId: deviceId,
    });

    return finalAnswer;
  };

export default calculator;

class PatternMatch<T extends object, R> {
  toMatch: T;
  internalAnswer: R | null;

  constructor(matchee: T) {
    this.toMatch = matchee;
    this.internalAnswer = null;
  }

  when(matcher: Partial<T>, fn: R | ((matchee: T) => R)) {
    if (this.internalAnswer !== null) {
      return this;
    }

    if (isMatch(this.toMatch, matcher)) {
      if (isFunction(fn)) {
        this.internalAnswer = fn(this.toMatch);
      } else {
        this.internalAnswer = fn;
      }
    }

    return this;
  }

  either(matchers: Partial<T>[], fn: R | ((matchee: T) => R)) {
    matchers.forEach((m) => this.when(m, fn));

    return this;
  }

  answer() {
    return this.internalAnswer;
  }
}
