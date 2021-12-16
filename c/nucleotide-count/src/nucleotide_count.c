#include "alloc.h"
#include "nucleotide_count.h"

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>


static inline char *empty_string() {
  return calloc_or_die(1, 1);
}


char *count(const char *strand) {
  unsigned int A = 0, C = 0, G = 0, T = 0;

  if (strand == NULL) {
    return empty_string();
  }

  for (const char *p = strand; *p; p++) {
    switch (*p) {
    case 'A':
      A++;
      break;
    case 'C':
      C++;
      break;
    case 'G':
      G++;
      break;
    case 'T':
      T++;
      break;
    default:
      return empty_string();
    }
  }

  char *result = NULL;
  const char *result_fmt = "A:%u C:%u G:%u T:%u";
  int result_size;

  /* check for necessary size */
  result_size = snprintf(result, 0, result_fmt, A, C, G, T);
  if (result_size < 0) {
    exit(1);
  }
  result_size++;
  result = malloc_or_die(result_size);
  if (snprintf(result, result_size, result_fmt, A, C, G, T) < 0) {
    free(result);
    exit(1);
  }
  return result;
}
