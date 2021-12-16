#include "square_root.h"

#include <stdint.h>


#ifdef __GNUC__
static inline uint16_t significant_bits_count(uint16_t x) {
  return x ? sizeof(int) * 8 - __builtin_clz((int)x) : 0;
}
#else
static uint16_t significant_bits_count(uint16_t x);
#endif

static inline uint16_t approximate_sqrt(uint16_t radicand, uint16_t x);

uint16_t squareRoot(uint16_t radicand) {
  // get a first rough approximation for the sqare root
  uint16_t seed = (significant_bits_count(radicand) + 1) / 2;

  // iteratively improve the approximation until integer part is not improving
  uint16_t prev, next = seed;
  do {
    prev = next;
    next = approximate_sqrt(radicand, prev);
  } while( next != prev);
  return next;
}


static inline uint16_t approximate_sqrt(uint16_t radicand, uint16_t x) {
  return (x + radicand / x) / 2;
}


#ifndef __GNUC__
static inline uint16_t significant_bits_count(uint16_t x) {
  uint16_t count = 16;
  if (x) {
    for (count = 0; ! (x & 0x8000) ; count++) {
      x <<= 1;
    }
  }
  return 16 - count;
}
#endif
