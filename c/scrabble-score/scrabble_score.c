#include <ctype.h>
#include "scrabble_score.h"

static unsigned int SCOREMAP[] = {1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10};

unsigned int score(const char *word) {
  unsigned int total = 0;
  while(word && *word) {
    total += SCOREMAP[toupper(*word++) - 'A'];
  }
  return total;
}
