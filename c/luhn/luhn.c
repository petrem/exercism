#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#include "luhn.h"

bool luhn(const char *num) {
  int shift = 0, n_digits = 0, sum = 0;
  for (const char *p = num + strlen(num) - 1; p >= num; p--) {
    if (isdigit(*p)) {
      int luhn_doubled = (*p - '0') << shift;
      sum += luhn_doubled > 9 ? luhn_doubled - 9 : luhn_doubled;
      shift ^= 0x01;
      n_digits++;
    } else if (*p != ' ') {
      return false;
    }
  }
  return n_digits >= 2 && sum % 10 == 0;
}
