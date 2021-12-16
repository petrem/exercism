#include <stdint.h>
#include <stdlib.h>
#include <math.h>

#include "rational_numbers.h"

#include <stdio.h>


static int16_t gcd(int16_t a, int16_t b);
static inline rational_t _absolute(rational_t r);
static inline int16_t copysign_int16(int16_t x, int16_t y);
static int16_t ipow(int16_t base, int16_t exp);

rational_t reduce(rational_t r) {
  int16_t sign = (r.numerator >= 0 && r.denominator > 0) ||\
    (r.numerator < 0 && r.denominator < 0) ? 1 : -1;
  rational_t ar = _absolute(r);
  int16_t gcd_ = gcd(ar.numerator, ar.denominator);
  printf("reduce %d / %d gcd: %d\n", r.numerator, r.denominator, gcd_);
  return (rational_t) {copysign_int16(ar.numerator, sign) / gcd_, ar.denominator / gcd_};
}

rational_t add(rational_t r1, rational_t r2) {
  return reduce((rational_t){
    r1.numerator * r2.denominator + r2.numerator * r1.denominator,
      r1.denominator * r2.denominator});
}

rational_t subtract(rational_t r1, rational_t r2) {
  return reduce((rational_t){
    r1.numerator * r2.denominator - r2.numerator * r1.denominator,
      r1.denominator * r2.denominator});
}

rational_t multiply(rational_t r1, rational_t r2) {
  return reduce((rational_t){
    r1.numerator * r2.numerator, r1.denominator * r2.denominator});
}

rational_t divide(rational_t r1, rational_t r2) {
  return reduce((rational_t){
    r1.numerator * r2.denominator,
      r1.denominator * r2.numerator});
}

rational_t absolute(rational_t r) {
  return reduce(_absolute(r));
}

rational_t exp_rational(rational_t r, int16_t n) {
  return reduce(n >= 0 ?
                (rational_t){ipow(r.numerator,  n), ipow(r.denominator, n)} :
                (rational_t){ipow(r.denominator, n), ipow(r.numerator, n)});
}

//todo
float exp_real(float x, rational_t r) {
  return r.numerator == 0 ? \
    1.0 : powf(x, (float)r.numerator / r.denominator);
}

static int16_t gcd(int16_t a, int16_t b) {
  if (b == 0) {
    return a;
  }
  return gcd(b, a % b);
}

static inline rational_t _absolute(rational_t r) {
  return (rational_t){abs(r.numerator), abs(r.denominator)};
}

static inline int16_t copysign_int16(int16_t x, int16_t y) {
  /* negate when the two sign bits are different */
  return (x ^ y) & 0xa000 ? -x : x;
}

static int16_t ipow(int16_t base, int16_t exp) {
  uint16_t result = 1;
  for (;;) {
    if (exp & 1) {
      result *= base;
    }
    exp >>= 1;
    if (!exp) {
      break;
    }
    base *= base;
  }
  return result;
}
