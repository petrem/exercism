#include "resistor_color_trio.h"

#include <math.h>

static inline uint16_t pow10(uint16_t n);

resistor_value_t colorCode(resistor_band_t code[3]) {
  resistor_value_t resistence;
  unsigned exponent;
  if (code[1] == BLACK) {
    resistence.value = code[0];
    exponent = 1 + code[2];
  } else {
    resistence.value = code[0] * 10 + code[1];
    exponent = code[2];
  }
  if (exponent >= KILOOHMS) {
    resistence.value *= pow10(exponent -  KILOOHMS);
    resistence.unit = KILOOHMS;
  } else {
    resistence.value *= pow10(exponent);
    resistence.unit = OHMS;
  }
  return resistence;
}

static inline uint16_t pow10(uint16_t n) {
  uint16_t pow10 = 1;
  for (; n--;)
    pow10 *= 10;
  return pow10;
}
