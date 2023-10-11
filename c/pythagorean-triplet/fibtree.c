#include <stdlib.h>
#include <errno.h>
#include "pythagorean_triplet.h"
#include "fibtree.h"

/* triangle functions */

triplet_op_int_t triangle_perimeter(op_triplet_t triangle) {
  return triangle.a + triangle.b + triangle.c;
}

/* Increments multiplier pointed to by `m` and returns the corresponding triangle. */
op_triplet_t triangle_generate_multiples(op_triplet_t triangle, triplet_op_int_t *m) {
  return (op_triplet_t){triangle.a * ++(*m), triangle.b * *m, triangle.c * *m};
}


/* Create a fibbox from available values. A fibbox is a matrix of the form
   [ q q' ]
   [ p p' ]
   where p = q' + q and p' = q + p
 */

fibbox_t fibbox_make_with_p_prime(triplet_op_int_t q_prime, triplet_op_int_t p_prime) {
  return (fibbox_t){
    (p_prime - q_prime) / 2,
    q_prime,
    (q_prime + p_prime) / 2,
    p_prime
  };
}

fibbox_t fibbox_make_with_p(triplet_op_int_t q_prime, triplet_op_int_t p) {
  return (fibbox_t){
    p - q_prime,
    q_prime,
    p,
    2 * p - q_prime
  };
}

fibbox_t fibbox_make_with_q(triplet_op_int_t q_prime, triplet_op_int_t q) {
  return (fibbox_t){
    q,
    q_prime,
    q_prime + q,
    2 * q + q_prime};
}

/* perimeter of the triangle made from the corresponding pythagorean triple */
triplet_op_int_t fibbox_perimeter(fibbox_t box) {
  return (box.q + box.p) * (box.q_prime + box.p_prime);
}

/* return the pythagorean triplet corresponding to this fibbox */
op_triplet_t fibbox_triplet(fibbox_t box) {
  triplet_op_int_t a, b, c;
  
  a = 2 * box.q * box.p;
  b = box.q_prime * box.p_prime;
  c = box.q * box.p_prime + box.q_prime * box.p;

  return a < b ? (op_triplet_t){a, b, c} : (op_triplet_t){b, a, c};
}


/* fibtree: a tree where each node is a fibbox that branches out
   into the three possible children.
*/

fibbox_t fibtree_get_child1(fibbox_t box) {
  return fibbox_make_with_p(box.q_prime, box.p_prime);
}

fibbox_t fibtree_get_child2(fibbox_t box) {
  return fibbox_make_with_q(box.q_prime, box.p_prime);
}

fibbox_t fibtree_get_child3(fibbox_t box) {
  return fibbox_make_with_q(box.p_prime, box.q_prime);
}

void fibtree_walk_depth_first(fibbox_t box,
                              bool (*visitor)(fibbox_t, void *),
                              void *payload) {
  /* A recursive implementation hits the stack limit at about depth 280.
     This makes it unsuitable for large perimeters.
   */
  fibbox_t stack[TRIPLET_MAX_STACK_LEVEL];
  int sp = 1;

  stack[0] = box;
  
  while (sp > 0 && sp < TRIPLET_MAX_STACK_LEVEL - 3) {
    /* pop */
    box = stack[--sp];
    if (visitor(box, payload)) {
      /* push children */
      stack[sp++] = fibtree_get_child3(box);
      stack[sp++] = fibtree_get_child2(box);
      stack[sp++] = fibtree_get_child1(box);
    }
  }
  
  if (sp > 0) {
    /* maximum stack depth reached, we couldn't expore the entire tree within
       the given cutoff value */
    exit(ENOMEM);
  }
}

