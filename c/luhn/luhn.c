#include <ctype.h>
#include <string.h>

#include "luhn.h"

bool luhn(const char *num) {
  int n_digits = 0, sum = 0;
  
  for (const char *p = num + strlen(num) - 1; p >= num; p--)
    if (isdigit(*p)) {
      int luhn_doubled = (*p - '0') << (n_digits++ & 0x01);
      sum += luhn_doubled > 9 ? luhn_doubled - 9 : luhn_doubled;
    } else if (*p != ' ') return false;
  
  return n_digits >= 2 && sum % 10 == 0;
}
