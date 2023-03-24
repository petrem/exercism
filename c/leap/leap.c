#include "leap.h"
#include <stdbool.h>

static inline bool is_divisible_by(unsigned n, unsigned m) {
  return n % m == 0;
}

bool is_leap_year(unsigned year) {
  return is_divisible_by(year, 4) && (!is_divisible_by(year, 100) || is_divisible_by(year, 400));
}
