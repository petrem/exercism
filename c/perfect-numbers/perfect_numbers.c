#include "perfect_numbers.h"

static long alicot_sum(int n);
static inline int cmp(long x, long y);

kind classify_number(int n) {
  if (n < 1)
    return ERROR;
  if (n == 1)
    return DEFICIENT_NUMBER;
  switch (cmp(alicot_sum(n), n)) {
  case -1:
    return DEFICIENT_NUMBER;
  case 0:
    return PERFECT_NUMBER;
  case 1:
    return ABUNDANT_NUMBER;
  default:
    return ERROR;
  }
}

static long alicot_sum(int n) {
  /* assert n > 1 */
  long acc = 1;
  for (int k = 2; k < n; k++)
    if (n % k == 0)
      acc += k;
  return acc;
}

static inline int cmp(long x, long y) {
  return x < y ? -1 : x > y ? 1 : 0;
}
