#include "largest_series_product.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

int64_t largest_series_product(char *digits, size_t span) {
  if (span == (size_t)-1)   // this seems rather dumb
    return -1;
  size_t n_digits = strlen(digits);
  if (span > n_digits)
    return -1;
  if (span > 0 && n_digits == 0)
    return -1;
  if (span == 0)
    return 1; // or should we not, for "000"?
  unsigned long *numbers = (unsigned long *)malloc(n_digits * sizeof(unsigned long));
  if (numbers == NULL)
    return -1;
  for (size_t k = 0; k < n_digits; k++) {
    if (!isdigit(digits[k]))
      return -1;
    numbers[k] = digits[k] - '0';
  }
  unsigned long max_product = 0;
  for (size_t from = 0; from <= n_digits - span; from++) {
    unsigned long prod = 1;
    for (size_t k = 0; k < span; k++)
      prod = prod * numbers[from+k];
    if (prod > max_product)
      max_product = prod;
  }
  return max_product;
}
