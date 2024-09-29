#include "rotational_cipher.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

char *rotate(const char *text, int shift_key) {
  char *rotated;
  // would love to use strnlen() instead, but that's not conforming to C99
  if (!text || !(rotated = (char *)malloc(strlen(text) + 1))) return NULL;
  char *p = rotated;
  while ((*p++ = isupper(*text) ? (*text++ + shift_key - 13) % 26 + 'A' : islower(*text) ? (*text++ + shift_key - 19) % 26 + 'a' : *text++));
  return rotated;
}
