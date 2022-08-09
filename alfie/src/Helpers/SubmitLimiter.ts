type milliseconds = number;

export interface SubmitLimiter {
  firstAttempt: milliseconds | null;
  lastAttempt: milliseconds | null;
  attemptCount: number;
}

export const limitIsExceeded = (
  limiter: SubmitLimiter,
  maxAttempts = 3
): boolean => {
  if (limiter.firstAttempt === null) {
    limiter.firstAttempt = Date.now();
  }
  limiter.lastAttempt = Date.now();

  const shouldResetLimiter =
    limiter.lastAttempt - limiter.firstAttempt > 1 * 60 * 1000; /* 10 min */
  if (shouldResetLimiter) {
    limiter.firstAttempt = Date.now();
    limiter.lastAttempt = Date.now();
    limiter.attemptCount = 0;
  }

  limiter.attemptCount++;

  if (__DEV__) {
    return false;
  }

  return limiter.attemptCount > maxAttempts;
};
