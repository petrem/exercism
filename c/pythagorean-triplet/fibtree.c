#include <stdlib.h>
#include "pythagorean_triplet.h"
#include "fibtree.h"

/* triangle functions */

triplet_int_t triangle_perimeter(triplet_t triangle) {
  return triangle.a + triangle.b + triangle.c;
}

/* Increments multiplier pointed to by `m` and returns the corresponding triangle. */
triplet_t triangle_generate_multiples(triplet_t triangle, triplet_int_t *m) {
  return (triplet_t){triangle.a * ++(*m), triangle.b * *m, triangle.c * *m};
}


/* fibbox functions */

fibbox_t fibbox_make_with_p_prime(triplet_int_t q_prime, triplet_int_t p_prime) {
  return (fibbox_t){
    (p_prime - q_prime) / 2,
    q_prime,
    (q_prime + p_prime) / 2,
    p_prime
  };
}

fibbox_t fibbox_make_with_p(triplet_int_t q_prime, triplet_int_t p) {
  return (fibbox_t){
    p - q_prime,
    q_prime,
    p,
    2 * p - q_prime
  };
}

fibbox_t fibbox_make_with_q(triplet_int_t q_prime, triplet_int_t q) {
  return (fibbox_t){
    q,
    q_prime,
    q_prime + q,
    2 * q + q_prime
  };
}

triplet_int_t fibbox_perimeter(fibbox_t box) {
  return (box.q + box.p) * (box.q_prime + box.p_prime);
}

triplet_t fibbox_triplet(fibbox_t box) {
  triplet_int_t a, b, c;
  a = 2 * box.q * box.p;
  b = box.q_prime * box.p_prime;
  c = box.q * box.p_prime + box.q_prime * box.p;
  if (a < b) {
    return (triplet_t){a, b, c};
  } else {
    return (triplet_t){b, a, c};
  }
}


/* fibtree functions */

fibbox_t fibtree_get_child1(fibbox_t box) {
  return fibbox_make_with_p(box.q_prime, box.p_prime);
}

fibbox_t fibtree_get_child2(fibbox_t box) {
  return fibbox_make_with_q(box.q_prime, box.p_prime);
}

fibbox_t fibtree_get_child3(fibbox_t box) {
  return fibbox_make_with_q(box.p_prime, box.q_prime);
}

void fibtree_walk_depth_first(fibbox_t box, bool (*visitor)(fibbox_t, void *), void *payload) {
  if (visitor(box, payload)) {
    fibtree_walk_depth_first(fibtree_get_child1(box), visitor, payload);
    fibtree_walk_depth_first(fibtree_get_child2(box), visitor, payload);
    fibtree_walk_depth_first(fibtree_get_child3(box), visitor, payload);
  }
}
