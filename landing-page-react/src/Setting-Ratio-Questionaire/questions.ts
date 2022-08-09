const questions = [
  {
    id: "status",
    style: "single-player",
    question: "What's your relationship status",
    answers: [
      { text: "Dating", id: "dating" },
      { text: "Living together", id: "together" },
      { text: "Married", id: "married" },
      { text: "Other", id: "other" },
    ],
  },
  {
    id: "dining-out",
    style: "single-player",
    question: "How do you split the bill dining out?",
    answers: [
      { text: "One person always pays the entire amount", id: "one-pays" },
      {
        text: "We split dining right down the middle, each pays half",
        id: "half-half",
      },
      {
        text: "We take turns paying the entire amount every dinner",
        id: "half-turns",
      },
      {
        text: "One person pays a little bit more of the bill than the other",
        id: "inequal",
      },
    ],
  },
  {
    id: "food-delivery",
    style: "single-player",
    question: "How do you split the bill when getting food delivery?",
    answers: [
      { text: "One person always pays the entire amount", id: "one-pays" },
      {
        text: "We split dining right down the middle, each pays half",
        id: "half-half",
      },
      {
        text: "We take turns paying the entire amount every dinner",
        id: "half-turns",
      },
      {
        text: "One person pays a little bit more of the bill than the other",
        id: "inequal",
      },
    ],
  },
  {
    id: "utility-bills",
    style: "single-player",
    question: "How do you split utility bills over the entire month?",
    answers: [
      { text: "One person always pays all the utility bills", id: "one-pays" },
      {
        text: "We split each bill half/half",
        id: "half-half",
      },
      {
        text: "Each person pays an entire bill but we take turns paying bills, so it evens out to half/half",
        id: "half-turns",
      },
      {
        text: "One person payes a bit more of the total bill each month",
        id: "inequal",
      },
    ],
  },
  {
    id: "rent",
    style: "single-player",
    question: "How do you split rent?",
    answers: [
      { text: "We each pay half the rent", id: "rent-half" },
      {
        text: "One of us pays just a little more than the other",
        id: "rent-inequal-small",
      },
      {
        text: "One of us pays more than the other",
        id: "rent-inequal-medium",
      },
      {
        text: "One of us pays all of the rent or a lot more of the rent",
        id: "rent-inequal-large",
      },
    ],
  },
  {
    id: "self-earn",
    style: "multi-player",
    question: "How much do you earn per year?",
    answers: [
      { text: "Less than $40,000", id: "less-40" },
      {
        text: "Between $40,000 and $70,000",
        id: "40-to-70",
      },
      {
        text: "Between $70,0000 and $130,000",
        id: "70-to-130",
      },
      {
        text: "More than $130,000",
        id: "more-130",
      },
    ],
  },
  {
    id: "partner-earn",
    style: "multi-player",
    question: "How much does your partner earn per year?",
    answers: [
      { text: "Less than $40,000", id: "less-40" },
      {
        text: "Between $40,000 and $70,000",
        id: "40-to-70",
      },
      {
        text: "Between $70,0000 and $130,000",
        id: "70-to-130",
      },
      {
        text: "More than $130,000",
        id: "more-130",
      },
    ],
  },
] as const;

export default questions;
