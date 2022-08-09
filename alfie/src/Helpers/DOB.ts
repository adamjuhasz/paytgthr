export const dobVerifier = (dob: string): string | null => {
  const parts = dob.split("/");
  if (parts.length !== 3) {
    return null;
  }

  const [month, day, year] = parts;
  const parsedMonth = parseInt(month, 10);
  const parsedDay = parseInt(day, 10);
  const parsedYear = parseInt(year, 10);

  if (parsedMonth < 1 || parsedMonth > 12) {
    return null;
  }

  if (parsedDay < 1 || parsedDay > 31) {
    return null;
  }

  // too old
  if (parseInt(year, 10) <= 1921) {
    return null;
  }

  const today = new Date();
  today.setHours(0, 0, 0, 0);

  const dobDate = new Date();
  dobDate.setHours(0, 0, 0, 0);
  dobDate.setFullYear(parsedYear);
  dobDate.setMonth(parsedMonth - 1, parsedDay);

  if (dobDate.getDate() !== parsedDay) {
    return null;
  }

  const eighteenInMs = 569203200000; // 18 leap years in ms
  if (today.getTime() - dobDate.getTime() < eighteenInMs) {
    return null;
  }

  return dob;
};
