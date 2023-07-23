#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>

#include "pythagorean_triplet.h"
#include "fibtree.h"

#define _DEBUG_TRIPLETS 1

#if _DEBUG_TRIPLETS
#include <stdio.h>
#endif

struct visitor_args {
  triplets_t *results;
  triplet_int_t cutoff;
  size_t *max_results;
};

static triplets_t *add_result(triplet_t triplet, triplets_t *result, size_t *max_results);
static bool visit(fibbox_t primitive_box, void *payload);


triplets_t *triplets_with_sum(triplet_int_t sum) {
#if _DEBUG_TRIPLETS
  printf("Triplets with sum %d\n", sum);
#endif
  size_t max_results = 1;
  triplets_t *results = malloc(sizeof(triplets_t) + max_results * sizeof(triplet_t));
  if (results == NULL) {
    exit(ENOMEM);
  }
  results -> count = 0;
  if (sum < 12) {
    /* smallest pythagorean triplet is 3, 4, 5 */
    return results;
  }

  fibbox_t first_box = fibbox_make_with_p_prime(1, 3);
  struct visitor_args payload = {results, sum, &max_results};
  fibtree_walk_depth_first(first_box, visit, &payload);
  return results;
}

void free_triplets(triplets_t *triplets) {
  if (triplets != NULL) {
    free(triplets);
  }
}

/* visit generated primary boxes, storing results if the corresponding triangle's
   perimeter is less than or equal to the cutoff value.

   Return true in this case -- meaning the subsequent primitive boxes could still make
   it under the cutoff, otherwise false.
*/
static bool visit(fibbox_t primitive_box, void *payload) {
#if _DEBUG_TRIPLETS
  printf("Visiting (%d, %d, %d, %d) with perimeter %d\n",
         primitive_box.q, primitive_box.q_prime,
         primitive_box.p, primitive_box.p_prime,
         fibbox_perimeter(primitive_box));
#endif
  struct visitor_args *args = (struct visitor_args *)payload;
  if (fibbox_perimeter(primitive_box) <= args->cutoff) {
    triplet_int_t multiplier = 1;
    triplet_t primitive_triplet = fibbox_triplet(primitive_box);
    triplet_int_t perimeter = triangle_perimeter(primitive_triplet);
    triplet_t triangle = primitive_triplet;
    while (perimeter <= args->cutoff) {
#if _DEBUG_TRIPLETS
      printf("Multiple %d: (%d, %d, %d) with perimeter %d\n",
             multiplier, triangle.a, triangle.b, triangle.c, perimeter);
#endif
      if (perimeter == args->cutoff) {
        args->results = add_result(triangle, args->results, args->max_results);
      }
      triangle = triangle_generate_multiples(primitive_triplet, &multiplier);
      perimeter = triangle_perimeter(triangle);
    };
    return true;
  }
  return false;
}

static triplets_t *add_result(triplet_t triplet, triplets_t *result, size_t *max_results) {
  triplets_t *new_result;
#if _DEBUG_TRIPLETS
  printf("Adding result #%zu (%d, %d, %d)\n",
         result->count + 1, triplet.a, triplet.b, triplet.c);
#endif

    if (result->count >= *max_results) {
#if _DEBUG_TRIPLETS
    printf("Reallocating results array from %zu to %zu\n", *max_results, *max_results * 2);
#endif
    *max_results *= 2;
    size_t new_size = sizeof(triplets_t) + *max_results * sizeof(triplet_t);
    new_result = realloc(result, new_size);
    if (new_result == NULL) {
      exit(ENOMEM);
    }
  } else {
    new_result = result;
  }
  new_result->triplets[new_result->count++] = triplet;
  return new_result;
}
