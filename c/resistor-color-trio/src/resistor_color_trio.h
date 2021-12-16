#ifndef RESISTOR_COLOR_TRIO_H
#define RESISTOR_COLOR_TRIO_H

#include <stdint.h>

typedef enum {
    BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE
} resistor_band_t;

typedef enum {
    OHMS=1,
    KILOOHMS=3
} resistence_unit_t;

typedef struct {
  uint16_t value;
  resistence_unit_t unit;
} resistor_value_t;

resistor_value_t colorCode(resistor_band_t code[3]);

#endif
