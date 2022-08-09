import { useLocalStorage } from "../Hooks/UseLocalStorage";
import calculator, { Answers } from "./Calculator";

interface Props {
  answers: Answers;
}

export default function SinglePlayerResults(props: Props): JSX.Element {
  const [deviceID] = useLocalStorage("deviceId", "");
  const answer = calculator(deviceID)(props.answers);

  return (
    <div className="prose prose-lg w-full mb-4">
      <h1>Our recomendation</h1>
      <p>
        Based on your answers and our data it looks like the following split
        would be the most fair for you two:
      </p>
      <h3 className="text-center ">{answer}</h3>
      <p>
        Feel free to click the "Prev" button and play around with your answers
        to see how it changes your recommendation
      </p>
      <p>
        Give us a bit more data on you and your partner's income and we can
        refine the recommendation!
      </p>
      <p>
        Does this not make sense? <a href="mailto:hi@paytgthr.com">Email us</a>{" "}
        and let us know.
      </p>
    </div>
  );
}
