#include "reverse_string.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>


char *reverse(const char *value) {
  if (value == NULL) {
    return NULL;
  } else {
    size_t reversed_len = strlen(value);
    char *reversed = (char *)malloc(reversed_len + 1);
    if (reversed == NULL) {
      exit(ENOMEM);
    }
    reversed += reversed_len;
    *reversed = '\0';
    while (*value) *--reversed = *value++;
    return reversed;
  }
}
