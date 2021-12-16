#include "armstrong_numbers.h"

#include <stdbool.h>
#include <math.h>
#include <stdio.h>


static inline int digits(unsigned n);


bool isArmstrongNumber(unsigned n) {
  unsigned long sum = 0;
  int n_left = n;

  if (n == 0 || n == 1)
    return true;

  for (int n_digits = digits(n); n_digits--; n_left /= 10) {
    sum += (unsigned long)pow(n_left % 10, digits(n));
  }

  return sum == n;
}


static inline int digits(unsigned n) {
  return (int)ceil(log10(n));
}
