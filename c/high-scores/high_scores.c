#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "high_scores.h"

#define TOP_SCORES_ARRAY_SIZE 3

#if TOP_SCORES_ARRAY_SIZE != 3
#error "Implementation assumes top of 3"
#endif

static inline void sort3_into(const int32_t a[3], int32_t b[3]);
static inline void insert_top_3(int32_t top[3], int32_t score);

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
  switch (scores_len) {
  case 1:
    output[0] = scores[0];
    return 1;
  case 2:
    if (scores[0] < scores[1]) {
      output[0] = scores[1];
      output[1] = scores[0];
    }
    return 2;
  }
  sort3_into(scores, output);
  for (size_t k = TOP_SCORES_ARRAY_SIZE; k < scores_len; k++) {
    insert_top_3(output, scores[k]);
  }
  return 3;
}


/* 
   Hopefully fastest way to move three elements into another array,
   sorted in reversed order.
*/
static inline void sort3_into(const int32_t a[3], int32_t b[3]) {
  if (a[0] < a[1]) {
    if (a[1] < a[2]) {
      b[0] = a[2];
      b[1] = a[1];
      b[2] = a[0];
    } else
      if (a[0] < a[2]) {
        b[0] = a[1];
        b[1] = a[2];
        b[2] = a[0];
      } else {
        b[0] = a[1];
        b[1] = a[0];
        b[2] = a[2];
      }
  } else {
    if (a[0] < a[2]) {
      b[0] = a[2];
      b[1] = a[0];
      b[2] = a[1];
    } else
      if (a[2] < a[1]) {
        b[0] = a[0];
        b[1] = a[1];
        b[2] = a[2];
    } else {
      b[0] = a[0];
      b[1] = a[2];
      b[2] = a[1];
    }
  }
}


enum {TOP_MIN=2, TOP_MID=1, TOP_MAX=0};

/* insert score in the reverse-sorterd array a, discarding the least value */
static inline void insert_top_3(int32_t top[3], int32_t score) {
  if (score > top[TOP_MIN]) {
    if (score >= top[TOP_MAX]) {
      top[TOP_MIN] = top[TOP_MID];
      top[TOP_MID] = top[TOP_MAX];
      top[TOP_MAX] = score;
    } else
      if (score <= top[TOP_MID]) {
        top[TOP_MIN] = score;
      } else {
        top[TOP_MIN] = top[TOP_MID];
        top[TOP_MID] = score;
      }
  }
}

/*
static inline void swap(int32_t *a, int32_t *b) {
  int32_t tmp = *a;
  *a = *b;
  *b = tmp;
}

static inline void sort3_inplace(int32_t a[3]) {
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
    } else
      if (a[2] < a[1]) {
        swap(&a[0], &a[2]);
    } else {
      int32_t tmp = a[0];
      a[0] = a[1];
      a[1] = a[2];
      a[2] = tmp;
    }
  }
}
*/
