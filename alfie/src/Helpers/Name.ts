export const defaultConfig = { allowWhitespace: false, minimumLength: 1 };

export const nameIsValid = (
  name: string,
  { allowWhitespace, minimumLength } = defaultConfig
): boolean => {
  // eslint-disable-next-line no-useless-escape
  const regex = /^[^\d!@#$%^&*()+=~`\"?<>.,/\\\[\]\{\}\|;:_]+$/;
  const noWhitespace = allowWhitespace ? true : /^[^\s]+$/.test(name);

  return regex.test(name) && noWhitespace && name.length >= minimumLength;
};
