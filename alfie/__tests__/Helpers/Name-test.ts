import {nameIsValid} from '../../src/Helpers/Name';

describe('Validates names', () => {
  it('allows correct name with no spaces', () => {
    expect(nameIsValid('Adam')).toEqual(true);
  });

  it('allows correct name with spaces', () => {
    expect(
      nameIsValid('Adam Bob', {allowWhitespace: true, minimumLength: 0}),
    ).toEqual(true);
  });

  it('disallows spaces by default', () => {
    expect(nameIsValid('Adam Bob')).toEqual(false);
  });

  it('does not allow "@" character', () => {
    expect(nameIsValid('Adam@me')).toEqual(false);
  });

  it('does not allow "." character', () => {
    expect(nameIsValid('Adamme.com')).toEqual(false);
  });

  it('does allow "-"', () => {
    expect(nameIsValid('Adam-bob')).toEqual(true);
  });

  it('does allow "\'"', () => {
    expect(nameIsValid("O'Connel")).toEqual(true);
  });

  it('does not allow empty string', () => {
    expect(nameIsValid('')).toEqual(false);
  });

  it('enforces minimum length', () => {
    expect(
      nameIsValid('adc', {allowWhitespace: true, minimumLength: 4}),
    ).toEqual(false);
    expect(
      nameIsValid('adcde', {allowWhitespace: true, minimumLength: 4}),
    ).toEqual(true);
  });
});
