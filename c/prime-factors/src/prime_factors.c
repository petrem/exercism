#include "prime_factors.h"

#include <stdint.h>
#include <stddef.h>
#include <setjmp.h>

#include <stdio.h>


typedef struct {
  uint64_t *factors;
  size_t n_factors;
  size_t maxfactors;
  uint64_t n;
  jmp_buf env;
} factor_context;

typedef void (*divisors_callback)(uint64_t, void *);

static void divisors(divisors_callback, void *);
static void determine_factors(uint64_t, void *);
static void add_factor(uint64_t, factor_context *);

size_t find_factors(uint64_t n, uint64_t factors[static MAXFACTORS]) {
  if (n > 1) {
    factor_context fc = {
        .factors = factors,
        .n_factors = 0,
        .maxfactors = MAXFACTORS,
        .n = n
    };
    if (setjmp(fc.env) == 0)
      divisors(determine_factors, &fc);
    else
      return fc.n_factors;
  }
  return 0;
}

static uint64_t KNOWN_PRIMES[] = {2, 3, 5, 7, 11, 13, 17, 23};
static unsigned KNOWN_PRIMES_COUNT = sizeof(KNOWN_PRIMES) / sizeof(uint64_t);


static void divisors(divisors_callback callback, void *context) {
  for (unsigned i = 0; i < KNOWN_PRIMES_COUNT; i++)
    callback(KNOWN_PRIMES[i], context);
  for (uint64_t next = KNOWN_PRIMES[KNOWN_PRIMES_COUNT - 1] + 2 ;; next +=2)
    callback(next, context);
}

static void determine_factors(uint64_t q, void *context) {
  factor_context *fc = (factor_context *)context;

  uint64_t r;
  for (; (fc->n % q) == 0; fc->n = r) {
    r = fc->n / q;
    add_factor(q, fc);
    if (q * q > r) {
      if (r != 1)
        add_factor(r, fc);
      longjmp(fc->env, 1);
    }
  }
}

static void add_factor(uint64_t factor, factor_context *fc) {
  fc->factors[(fc->n_factors)++] = factor;
  if (fc->n_factors == fc->maxfactors)
    longjmp(fc->env, 1);
}
