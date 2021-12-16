#include "resistor_color.h"


uint16_t colorCode(resistor_band_t color) {
  return color;
}

const int *colors() {
  static const int all_colors[] = {
    BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE
  };
  return all_colors;
}
