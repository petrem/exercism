#include "roman_numerals.h"
#include <stdio.h>
#include <string.h>


int _main() {
  unsigned max = 0, len;
  char *max_num;
  unsigned max_n;
  for (unsigned n = 9999; n > 0; n--) {
    char *numeral = to_roman_numeral(n);
    len = strlen(numeral);
    //printf("%d: %s -> %d\n", n, numeral, len);
    if (max < len) {
      max = len;
      max_n = n;
      max_num = numeral;
    }
  }
  printf("Max repr: %d for %s (%d)\n", max, max_num, max_n);
  return 0;
}
