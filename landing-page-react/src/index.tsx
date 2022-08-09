import React from "react";
import { hydrate, render } from "react-dom";
import { Helmet } from "react-helmet";

import "./index.css";

import "@fontsource/rubik/300.css";
import "@fontsource/rubik/400.css";
import "@fontsource/rubik/500.css";
import "@fontsource/rubik/600.css";
import "@fontsource/rubik/700.css";
import "@fontsource/rubik/800.css";
import "@fontsource/rubik/900.css";
import "@fontsource/rubik/variable.css";

const App = () => (
  <React.StrictMode>
    <Helmet>
      {/*
          https://github.com/joshbuchea/HEAD#apple-ios
        */}
      <title>Self driving money for couples</title>
    </Helmet>
    <div className="prose prose-xl prose-rose max-w-5xl w-full mx-auto py-7 px-4">
      <h1>Thanks for all the fish</h1>
      <h2>Pay Tgthr is closing up shop</h2>
      <blockquote>
        It is with an exceedingly heavy heart that we write to inform you that
        Pay Tgthr, Inc. will be shutting down the Tgthr Card effective March 14,
        2022. After careful consideration and investigation of all possible
        options, we have determined that we will be unable to continue operating
        past this date.
        <br />
        <br />
        While this may be the end of our journey Tgthr, Adam and I would like to
        sincerely thank you all for your support. I know I've said it before,
        but without you our attempt to reimagine couple's banking could not have
        even gotten off the ground. It was fun while it lasted!
        <br />
        <br />
        We are grateful to you for being on this journey with us!
        <br />
        <br />- Mike and Adam
      </blockquote>
      <p>
        Need to dispute a purchase? We're still here at{" "}
        <a href="mailto:hi@paytgthr.com">hi@paytgthr.com</a>
      </p>

      <h2>What's next for us?</h2>
      <p>
        We're working on{" "}
        <a href="https://heywillow.io?utm_source=paytgthr">Willow</a>, a better
        way to do customer service
      </p>
    </div>
  </React.StrictMode>
);

const rootElement = document.getElementById("root");
if (rootElement !== null && rootElement.hasChildNodes()) {
  hydrate(<App />, rootElement);
} else {
  render(<App />, rootElement);
}

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
// reportWebVitals(console.log);
