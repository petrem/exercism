#include "sum_of_multiples.h"
#include "bst.h"
#include <math.h>
#include <stdio.h>

static void add_factor(struct BST *results, unsigned result);
static unsigned add(unsigned x, unsigned y);

unsigned sum_of_multiples(const unsigned multiples[], unsigned n_multiples, unsigned upto) {
  if (multiples == NULL) {
    return 0;
  }

  int n_factors = upto * n_multiples;
  for (unsigned i = 0; i < n_multiples; i++) {
    if (multiples[i] == 0)
      continue;
    n_factors += upto / multiples[i];
  }
  struct BST *factors = bst_init(n_factors);

  for (unsigned m = 0; m < n_multiples; m++) {
    if (multiples[m] == 0)
      continue;
    for (unsigned i = 1; multiples[m] * i < upto; i++) {
      add_factor(factors, multiples[m] * i);
    }
  }
  unsigned sum = bst_reduce(factors, add, 0);
  bst_destroy(factors);
  return sum;
}

static void add_factor(struct BST *factors, unsigned factor) {
  int pos = bst_insert(factors, factor);
  switch (pos) {
  case -2: /* size overflowed */
    if (0 == bst_resize(factors, 2 * bst_length(factors))) {
      /* try again */
      if(-1 != bst_insert(factors, factor)) {
        exit(1);
      }
    } else {
      /* error resizing */
      exit(1);
    }
    break;
  case -3:
    /* error inserting */
    exit(1);
  default:
    /* element was inserted or was already present */
    break;
  }
}

static inline unsigned add(unsigned x, unsigned y) {
  return x + y;
}
