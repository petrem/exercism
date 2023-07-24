#ifndef PYTHAGOREAN_TRIPLET_H
#define PYTHAGOREAN_TRIPLET_H

#include <stdlib.h>
#include <stdint.h>

#define TRIPLET_INTERFACE_INT_SIZE 16
/* define the working int type to be double that of the interface, to avoid overflows */
#define TRIPLET_INT_SIZE 32

/* The following attempts to detect incorrect integer sizes at compile time. Thus I took
   this approach of macro-expanding in the typedefs instead of simply declaring them.
   Maybe there's a better way...
*/

#if (TRIPLET_INT_SIZE) != 2 * (TRIPLET_INTERFACE_INT_SIZE)
#error "Operational int should be twice the interface int size"
#endif

#define _TRIPLET_MAKE_INT(_SIZE) uint ## _SIZE ## _t
#define TRIPLET_MAKE_INT(_SIZE) _TRIPLET_MAKE_INT(_SIZE)

typedef TRIPLET_MAKE_INT(TRIPLET_INTERFACE_INT_SIZE) triplet_int_t;
typedef TRIPLET_MAKE_INT(TRIPLET_INT_SIZE) triplet_op_int_t;

/* program interface types */
typedef struct {
  triplet_int_t a;
  triplet_int_t b;
  triplet_int_t c;
} triplet_t;
typedef struct {
  size_t count;
  triplet_t triplets[];
} triplets_t;

/* shadow structs for operational usage */
typedef struct {
  triplet_op_int_t a;
  triplet_op_int_t b;
  triplet_op_int_t c;
} op_triplet_t;
typedef struct {
  size_t count;
  op_triplet_t triplets[];
} op_triplets_t;

triplets_t *triplets_with_sum(triplet_int_t sum);
void free_triplets(triplets_t *triplets);

#endif
