#include "pythagorean_triplet.h"

#include <stdlib.h>
#include <stdbool.h>

/* the "obvious" solution */

static inline bool is_triplet(triplet_int_t a, triplet_int_t b, triplet_int_t c);


triplets_t *triplets_with_sum(triplet_int_t sum) {
  triplets_t *result = malloc(sizeof(triplets_t) + 100 * sizeof(triplet_t));
  result -> count = 0;
  for (triplet_int_t a=1; a < sum; a++) {
    for (triplet_int_t b=a; b < sum; b++) {
      if (is_triplet(a, b, sum - a - b)) {
        result->triplets[result->count++] = (triplet_t){a, b, sum - a - b};
      }
    }
  }
  return result;
}

void free_triplets(triplets_t *triplets) {
  if (triplets != NULL) {
    free(triplets);
  }
}


static inline bool is_triplet(triplet_int_t a, triplet_int_t b, triplet_int_t c) {
  return a * a + b * b == c * c;
}
