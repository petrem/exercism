#include "hamming.h"
#include <stddef.h>


int compute(const char *dna1, const char *dna2) {
  size_t distance = 0;
  size_t i;

  if (dna1 == NULL || dna2 == NULL) {
    return -1;
  }

  for (i = 0; dna1[i] != '\0' && dna2[i] != '\0'; i++) {
    if (dna1[i] != dna2[i]) {
      distance++;
    }
  }
  if (dna1[i] != '\0' || dna2[i] != '\0') {
    /* strings are not of equal length */
    return -1;
  }
  return distance;
}
