#include <stdlib.h>
#include <stdbool.h>
#include "pythagorean_triplet.h"

/* work with triangles */

triplet_op_int_t triangle_perimeter(op_triplet_t triangle);
op_triplet_t triangle_generate_multiples(op_triplet_t triangle, triplet_op_int_t *m);


/*
    A matrix representation of a generalized Fibonacci sequence
    (and pythagorean triple). The box looks like:
    [ q q']
    [ p p'] where
    p  = q' + q
    p' = q  + p

    To initialize a box you need to give it at least one of the following:
    q', p'
    q,  q'
    q', p
*/


typedef struct {
  triplet_op_int_t q, q_prime, p, p_prime; 
} fibbox_t;

/* constructors always take q_prime and one of the other values */
fibbox_t fibbox_make_with_p_prime(triplet_op_int_t q_prime, triplet_op_int_t p_prime);
fibbox_t fibbox_make_with_p(triplet_op_int_t q_prime, triplet_op_int_t p);
fibbox_t fibbox_make_with_q(triplet_op_int_t q_prime, triplet_op_int_t q);
triplet_op_int_t fibbox_perimeter(fibbox_t box);
op_triplet_t fibbox_triplet(fibbox_t box);


/*
  A ternary tree made of fibboxes. This generates all the primary pythagorean triples.
*/


typedef void (*box_visitor_fn_t)(fibbox_t, void *);

fibbox_t fibtree_get_child1(fibbox_t tree);
fibbox_t fibtree_get_child2(fibbox_t tree);
fibbox_t fibtree_get_child3(fibbox_t tree);

#define TRIPLET_MAX_STACK_LEVEL 5000

void fibtree_walk_depth_first(fibbox_t tree, bool (*visitor)(fibbox_t, void *), void *payload);
