#include "darts.h"
#include <stdint.h>
#include <math.h>

uint8_t score(coordinate_t pos) {
  float r = sqrt(pos.x * pos.x + pos.y * pos.y);
  return r > 10 ? 0 : r > 5 ? 1 : r > 1 ? 5 : 10;
}
