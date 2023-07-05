#ifndef PYTHAGOREAN_TRIPLET_H
#define PYTHAGOREAN_TRIPLET_H

#include <stdlib.h>
#include <stdint.h>

typedef uint16_t triplet_int_t;
typedef struct {
  triplet_int_t a;
  triplet_int_t b;
  triplet_int_t c;
} triplet_t;
typedef struct {
  size_t count;
  triplet_t triplets[];
} triplets_t;

triplets_t *triplets_with_sum(triplet_int_t sum);
void free_triplets(triplets_t *triplets);

#endif
