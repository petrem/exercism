#include "pangram.h"
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

bool is_pangram(const char *sentence) {
  bool used[26];
  if (sentence == NULL)
    return false;
  memset(used, false, sizeof(used));
  for (const char *p = sentence; *p; p++)
    if (isalpha(*p))
      used[tolower(*p) - 'a'] = true;
  for (unsigned long i = 0; i < sizeof(used); i++)
    if (!used[i])
      return false;
  return true;
}
