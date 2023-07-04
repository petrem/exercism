#include <stdlib.h>
#include <string.h>
#include "reverse_string.h"

char *reverse(const char *value) {
  if (value == NULL) {
    return NULL;
  } else {
    size_t reversed_len = strlen(value);
    char *reversed = malloc(reversed_len + 1);
    for (size_t j = 1; j <= reversed_len; j++) {
      reversed[j-1] = value[reversed_len - j];
    }
    reversed[reversed_len] = '\0';
    return reversed;
  }
}
