#include "rotational_cipher.h"

#include <ctype.h>

char *strdup(const char *s);

char *rotate(const char *text, int shift_key) {
  char *rotated = strdup(text), *p = rotated;
  while ((*p++ = isupper(*text) ? (*text++ + shift_key - 13) % 26 + 'A' : islower(*text) ? (*text++ + shift_key - 19) % 26 + 'a' : *text++));
  return rotated;
}
