export const formdataHeaders = (formdata: FormData): Record<string, string> => {
  // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-explicit-any
  if (typeof (formdata as any).getHeaders === "function") {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-call
    return { "Content-Type": (formdata as any).getHeaders()["content-type"] };
  }
  return {};
};
