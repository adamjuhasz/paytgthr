import { useLocalStorage } from "../Hooks/UseLocalStorage";
import calculator, { Answers } from "./Calculator";

interface Props {
  answers: Answers;
}

function convertToNum(str: Answers["status"]): number {
  switch (str) {
    case "less-40":
      return 1;

    case "40-to-70":
      return 2;

    case "70-to-130":
      return 3;

    case "more-130":
      return 4;

    default:
      return 0;
  }
}

export default function MultiPlayerResults(props: Props): JSX.Element {
  const [deviceID] = useLocalStorage("deviceId", "");
  let answer = calculator(deviceID)(props.answers);
  const self = convertToNum(props.answers["self-earn"]);
  const partner = convertToNum(props.answers["partner-earn"]);

  const diff = Math.abs(self - partner);

  if (diff > 2) {
    answer = "70:30";
  } else if (diff === 2) {
    switch (answer) {
      case "50:50":
        answer = "60:40";
        break;

      case "60:40":
        answer = "70:30";
        break;

      case "70:30":
        answer = "70:30";
        break;
    }
  }

  console.log(diff, self, partner);

  return (
    <div className="prose prose-lg w-full mb-4">
      <h1>Our recomendation</h1>
      <p>
        Based on your salary answers and our data it looks like the following
        split would be the most fair for you two:
      </p>
      <h3 className="text-center ">{answer}</h3>
      <p>
        Feel free to click the "Prev" button and play around with your answers
        to see how it changes your recommendation
      </p>
      <p>
        Does this not make sense? <a href="mailto:hi@paytgthr.com">Email us</a>{" "}
        and let us know.
      </p>
    </div>
  );
}
