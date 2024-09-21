#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>

#include "all_your_base.h"

/* TODO
   - operations are not checked -- overrun might occur
   - use operation that computes quotient and modulus (think divmod);
     see https://www.gnu.org/software/libc/manual/html_node/Integer-Division.html
*/

typedef long number_t;

static number_t from_base(base_t input_base, const digit_t digits[], size_t input_length);
static size_t to_base(base_t output_base, number_t n, digit_t digits[]);

size_t rebase(digit_t digits[], base_t input_base, base_t output_base, size_t input_length) {
  if (input_length == 0 || input_base < 2 || output_base < 2)
    goto error;
  number_t n = from_base(input_base, digits, input_length);
  if (n == -1)
    goto error;
  if (n == 0) {
    digits[0] = 0;
    return 1;
  }
  return to_base(output_base, n, digits);
 error:
  return 0;
}

static number_t from_base(base_t input_base, const digit_t digits[], size_t input_length) {
  number_t result = 0;
  number_t base_exp = 1;
  size_t k = input_length;
  do {
    k--;
    if (digits[k] < 0 || digits[k] >= input_base)
      goto error;
    result += digits[k] * base_exp;
    base_exp *= input_base;
  } while (k != 0);
  return result;
 error:
  return -1;
}

static size_t to_base(base_t output_base, number_t n, digit_t digits[]) {
  size_t k = 0, n_digits;
  digit_t reversed[DIGITS_ARRAY_SIZE];
  number_t quot = n;
  for (; quot > 0 && k < 64; quot /= output_base) {
    reversed[k++] = quot % output_base;
  }
  if (k==64 && quot != 0)
    return 0;

  n_digits = k;
  for(size_t j = 0; j < n_digits; j++)
    digits[j] = reversed[--k];

  return n_digits;
}
