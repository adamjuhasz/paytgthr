import { Linking } from "react-native";

export const sendEmail = async (
  subjectLine: string,
  body = ""
): // eslint-disable-next-line @typescript-eslint/no-explicit-any
Promise<any> => {
  const subjectLineNormed = subjectLine.replace(" ", "%20");
  const payTgthrEmail = "hi@paytgthr.com";

  return Linking.openURL(
    `mailto:${payTgthrEmail}?subject=${subjectLineNormed}&body=${body}`
  );
};
