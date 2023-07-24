#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>

#include "pythagorean_triplet.h"
#include "fibtree.h"

#define _DEBUG_LEVEL 0

#if _DEBUG_LEVEL > 0
#include <stdio.h>
#endif

struct visitor_args {
  op_triplets_t *results;
  triplet_int_t cutoff;
  size_t *max_results;
};

static op_triplets_t *add_result(op_triplet_t triplet, op_triplets_t *result, size_t *max_results);
static bool visit(fibbox_t primitive_box, void *payload);
static triplets_t *make_interface_results(op_triplets_t *results);


triplets_t *triplets_with_sum(triplet_int_t sum) {
#if _DEBUG_LEVEL > 0
  printf("Triplets with sum %d\n", sum);
#endif
  size_t max_results = 1;
  struct visitor_args payload = {NULL, sum, &max_results};
  payload.results = malloc(sizeof(op_triplets_t) + max_results * sizeof(op_triplet_t));

  if (payload.results == NULL) {
    exit(ENOMEM);
  }
  payload.results -> count = 0;
  if (sum >= 12) {
    /* smallest pythagorean triplet is 3, 4, 5 */
    fibbox_t first_box = fibbox_make_with_p_prime(1, 3);
    fibtree_walk_depth_first(first_box, visit, &payload);
  }
  triplets_t *ret_results = make_interface_results(payload.results);
  free(payload.results);
  return ret_results;
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
#if _DEBUG_LEVEL >= 2
  printf("Visiting (%d, %d, %d, %d) with perimeter %d\n",
         primitive_box.q, primitive_box.q_prime,
         primitive_box.p, primitive_box.p_prime,
         fibbox_perimeter(primitive_box));
#endif
  struct visitor_args *args = (struct visitor_args *)payload;
  if (fibbox_perimeter(primitive_box) <= args->cutoff) {
    triplet_op_int_t multiplier = 1;
    op_triplet_t primitive_triplet = fibbox_triplet(primitive_box);
    triplet_op_int_t perimeter = triangle_perimeter(primitive_triplet);
    op_triplet_t triangle = primitive_triplet;
    while (perimeter <= args->cutoff) {
#if _DEBUG_LEVEL >= 2
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

static op_triplets_t *add_result(op_triplet_t triplet,
                                 op_triplets_t *results,
                                 size_t *max_results) {
  op_triplets_t *new_results;
#if _DEBUG_LEVEL >= 1
  printf("Adding result #%zu (%d, %d, %d)\n",
         results->count + 1, triplet.a, triplet.b, triplet.c);
#endif

    if (results->count >= *max_results) {
#if _DEBUG_TRIPLETS >= 1
    printf("Reallocating results array from %zu to %zu\n", *max_results, *max_results * 2);
#endif
    *max_results *= 2;
    size_t new_size = sizeof(op_triplets_t) + *max_results * sizeof(op_triplet_t);
    new_results = realloc(results, new_size);
    if (new_results == NULL) {
      exit(ENOMEM);
    }
  } else {
    new_results = results;
  }
  new_results->triplets[new_results->count++] = triplet;
  return new_results;
}

triplets_t *make_interface_results(op_triplets_t *results) {
  triplets_t *ret_results = malloc(sizeof(op_triplets_t) +
                                   results->count * sizeof(triplet_t));
  if (ret_results == NULL) {
    exit(ENOMEM);
  }
  ret_results->count = results->count;
  for (size_t j = 0; j < results->count; j++) {
    op_triplet_t triplet = results->triplets[j];
    ret_results->triplets[j] = (triplet_t){
      (triplet_int_t)triplet.a,
      (triplet_int_t)triplet.b,
      (triplet_int_t)triplet.c};
  }
  return ret_results;
}
