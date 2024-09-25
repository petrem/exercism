#include "yacht.h"

#include <stdlib.h>

static void make_counts(dice_t dice, _counts table);

#define SINGLES(NAME, FACE)                      \
  int (NAME) (_counts counts) {                  \
    return (FACE) * counts[(FACE)];              \
  }

SINGLES(ONES,   1)
SINGLES(TWOS,   2)
SINGLES(THREES, 3)
SINGLES(FOURS,  4)
SINGLES(FIVES,  5)
SINGLES(SIXES,  6)

int CHOICE(_counts counts) {
  int sum = 0;
  for (size_t k = 1; k <= 6; k++) {
    sum += counts[k] * k;
  }
  return sum;
}

int FULL_HOUSE(_counts counts) {
  int first_count = 0, first_face = 0;
  int second_count = 0, second_face = 0;
  for (size_t k = 1; k <= 6; k++) {
    if (counts[k] > second_count) {
      second_count = counts[k];
      second_face = k;
    }
    if (second_count > first_count) {
      int tmp_count = first_count, tmp_face = first_face;
      first_count = second_count;
      first_face = second_face;
      second_count = tmp_count;
      second_face = tmp_face;
    }
  }
  if (first_count == 3 && second_count == 2)
    return CHOICE(counts);
  return 0;
}

#define YACHTISH(NAME, HOWMANY, SCORE)           \
  int (NAME) (_counts counts) {                  \
    for (size_t k = 1; k <= 6; k++) {            \
      if (counts[k] >= (HOWMANY)) {              \
        return SCORE(k);                         \
      }                                          \
    }                                            \
    return 0;                                    \
  }

#define YACHT_SCORE(HOLE) (50)
YACHTISH(YACHT, 5, YACHT_SCORE)

#define FOUR_OF_A_KIND_SCORE(NUMBER) (4 * (NUMBER))
YACHTISH(FOUR_OF_A_KIND, 4, FOUR_OF_A_KIND_SCORE)

#define STRAIGHTS(NAME, START)                          \
  int (NAME) (_counts counts) {                         \
    for (size_t k = (START); k <= (START) + 4; k++) {   \
      if (counts[k] != 1) return 0;                     \
    }                                                   \
    return 30;                                          \
  }
  
STRAIGHTS(LITTLE_STRAIGHT, 1)
STRAIGHTS(BIG_STRAIGHT, 2)


static void make_counts(dice_t dice, _counts table) {
  for (size_t k = 0; k < 5; k++) {
    if (dice.faces[k] < 1 || dice.faces[k] > 6)
      exit(EXIT_FAILURE);
    table[dice.faces[k]]++;
  }
}

int score(dice_t dice, category_t category) {
  _counts counts = {0};
  make_counts(dice, counts);
  return category(counts);
}
