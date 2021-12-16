#include "pangram.h"
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#define LETTERS_IN_ALPHABET 26

bool is_pangram(const char *sentence) {
  bool seen[LETTERS_IN_ALPHABET];
  unsigned used = 0;
  if (sentence == NULL)
    return false;
  memset(seen, false, sizeof(seen));
  for (const char *p = sentence; *p; p++)
    if (isalpha(*p)) {
      if (!seen[tolower(*p) - 'a']) {
        seen[tolower(*p) - 'a'] = true;
      }
      used++;
    }
  return used == LETTERS_IN_ALPHABET;
}
