#include <string.h>
#include "luhn.h"

bool luhn(const char *num) {
  int n_digits = 0, sum = 0;
  
  for (const char *p = num + strlen(num) - 1; p >= num; p--) {
    if (*p == ' ') continue;
    if (*p < '0' || *p > '9') return false;
    sum += ((*p - '0') << (n_digits & 0x01)) > 9 ? ((*p - '0') << (n_digits++ & 0x01)) - 9 : (*p - '0') << (n_digits++ & 0x01);
  }
  
  return n_digits >= 2 && sum % 10 == 0;
}
