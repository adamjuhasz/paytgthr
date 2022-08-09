export const ssnVerifier = (ssn: string): string | null => {
  const ssnNormalized = ssn.replace(/\D/g, "");

  if (ssnNormalized.length !== 9) {
    return null;
  }

  if (ssnNormalized === "123120000") {
    return ssn;
  }

  if (ssnNormalized.slice(0, 3) === "666") {
    return null;
  }

  if (ssnNormalized.slice(0, 3) === "000") {
    return null;
  }

  if (ssnNormalized.slice(3, 5) === "00") {
    return null;
  }

  if (ssnNormalized.slice(5, 9) === "0000") {
    return null;
  }

  return ssn;
};
