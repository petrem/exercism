#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "high_scores.h"

#define TOP_SCORES_ARRAY_SIZE (3)

static inline void swap(int32_t *a, int32_t *b);
static inline void sort3(int32_t a[3]);

/// Return the latest score.
int32_t latest(const int32_t *scores, size_t scores_len) {
  assert(scores != NULL);
  assert(scores_len > 0);
  return scores[scores_len - 1];
}

/// Return the highest score.
int32_t personal_best(const int32_t *scores, size_t scores_len) {
  assert(scores != NULL);
  assert(scores_len > 0);
  int32_t max = scores[0];
  for (const int32_t *p = scores + 1; p != scores + scores_len; p++)
    if (max < *p)
      max = *p;
  return max;
}

/// Write the highest scores to `output` (in non-ascending order).
/// Return the number of scores written.
size_t personal_top_three(const int32_t *scores,
                          size_t scores_len,
                          int32_t *output) {
  assert(scores != NULL);
  assert(scores_len > 0);
  int32_t top_scores[TOP_SCORES_ARRAY_SIZE];
  size_t k;
  for (k = 0; k < TOP_SCORES_ARRAY_SIZE; k++)
    top_scores[k] = scores[k];
  for (; k < scores_len; k++)
    continue;
}


static inline void insert_in_top(int32_t top[TOP_SCORES_ARRAY_SIZE], int32_t score) {
  size_t j;
  for (j = 0; j < TOP_SCORES_ARRAY_SIZE && score > top[j]; j++);
  for(size_t k = 0; k < j; k++) {
    top[k] = top[k+1];
  }
}

/* hopefully the fastest way to sort an array of three elements */
static inline void sort3(int32_t a[3]) {
  if (a[0] < a[1]) {
    if (a[1] < a[2]) {
      return;
    } else
      if (a[0] < a[2]) {
        swap(&a[1], &a[2]);
      } else {
        int32_t tmp = a[0];
        a[0] = a[2];
        a[2] = a[1];
        a[1] = tmp;
      }
  } else {
    if (a[0] < a[2]) {
      swap(&a[0], &a[1]);
    } else if (a[2] < a[1]) {
      swap(&a[0], &a[2]);
    } else {
      int32_t tmp = a[0];
      a[0] = a[1];
      a[1] = a[2];
      a[2] = tmp;
    }
  }
}

static inline void swap(int32_t *a, int32_t *b) {
  int32_t tmp = *a;
  *a = *b;
  *b = tmp;
}

/* insert k in the sorterd array a, discarding the least value */
static inline void insert_high_3(int32_t a[3], int32_t k) {
}
