#include <ctype.h>
#include <stdint.h>
#include <string.h>
#include "isogram.h"

static const unsigned int NUM_LETTERS = 26;

bool is_isogram(const char phrase[]) {
  uint8_t occurences[NUM_LETTERS];
  char *c;
  int idx;

  if (phrase == NULL)
    return false;

  memset(occurences, 0, NUM_LETTERS);
  for (c = (char *)phrase; *c; c++) {
    if (isalpha(*c)) {
      idx = tolower(*c) - 'a';
      if (occurences[idx] == 1)
        return false;
      occurences[idx] = 1;
    }
  }
  return true;
}
