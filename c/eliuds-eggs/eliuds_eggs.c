#include "eliuds_eggs.h"

unsigned int egg_count(unsigned long eggs) {
  unsigned int count = eggs & 1;
  while(eggs >>= 1) count += eggs & 1;
  return count;
}
