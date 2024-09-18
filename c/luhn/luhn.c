#include <string.h>
#include "luhn.h"

bool luhn(const char *num) {
  int n_digits = 0, sum = 0, luhn_double[] = {0, 2, 4, 6, 8, 1, 3, 5, 7, 9};
  
  for (const char *p = num + strlen(num) - 1; p >= num; p--) {
    if (*p == ' ') continue;
    if (*p < '0' || *p > '9') return false;
    sum += n_digits++ & 0x01 ? luhn_double[*p - '0'] : *p - '0';
  }
  
  return n_digits >= 2 && sum % 10 == 0;
}
