#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "bitvector.h"
#include "sieve.h"


uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes) {
  size_t n_primes = 0;
  WORDTYPE *marks = bitvector_init(limit + 1);
  uint32_t i = 2;
  for (; i * i <= limit; i++) {
    if (bitvector_get(marks, i) == UNMARKED) {
      primes[n_primes++] = i;
      if (n_primes == max_primes)
        break;
      for (uint32_t j = i * i; j <= limit ; j += i)
        bitvector_set(marks, j, NOT_PRIME);
    }
  }
  for (; i <= limit; i++) {
    if (bitvector_get(marks, i) == UNMARKED) {
      primes[n_primes++] = i;
      if (n_primes == max_primes)
        break;
    }
  }
  free(marks);
  return n_primes;
}
