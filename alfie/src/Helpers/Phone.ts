export const phoneVerifier = (phone: string): string | null => {
  const parts = phone.split("-");
  if (parts.length !== 3) {
    return null;
  }

  const [areacode] = parts;
  if (areacode.charAt(0) === "0" || areacode.charAt(0) === "1") {
    return null;
  }

  return phone;
};
