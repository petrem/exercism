#include "pythagorean_triplet.h"

#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>


/* simple, "obvious", solution, with realloc */

static inline bool is_triplet(triplet_int_t a, triplet_int_t b, triplet_int_t c);


triplets_t *triplets_with_sum(triplet_int_t sum) {
  size_t max_results = 8;
  triplets_t *result = malloc(sizeof(triplets_t) + max_results * sizeof(triplet_t));
  if (result == NULL) {
    exit(ENOMEM);
  }
  result -> count = 0;
  if (sum < 12) {
    /* smallest pythagorean triplet is 3, 4, 5 */
    return result;
  }
  for (triplet_int_t a = 3; a < sum - 3; a++) {
    for (triplet_int_t b = a + 1 ; b < sum - 2; b++) {
      if (is_triplet(a, b, sum - a - b)) {
        result->triplets[result->count++] = (triplet_t){a, b, sum - a - b};
        if (result->count >= max_results) {
          max_results *= 2;
          result = realloc(result, sizeof(triplets_t) + max_results * sizeof(triplet_t));
          if (result == NULL) {
            exit(ENOMEM);
          }
        }
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
