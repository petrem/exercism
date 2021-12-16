#include "phone_number.h"

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <ctype.h>

static inline void *calloc_or_die(size_t nmemb, size_t size);
static void make_error(char *number);

static const char acceptable_nonalpha[] = "() -.";

char *phone_number_clean(const char *phone_number) {
  char *cleaned = calloc_or_die(12, 1);
  char *q = cleaned;
  unsigned len = 0;

  /* skip initial '+', to avoid letting '+' anywhere else */
  if (phone_number[0] == '+')
    phone_number++;
  for (const char *p = phone_number; *p; p++) {
    if (isdigit(*p)) {
      *q++ = *p;
      len++;
      if (len == 12)
        break;
    }
    else if (strchr(acceptable_nonalpha, *p) == NULL)
      break;
  }
  if (len == 11 && cleaned[0] == '1') {
    memmove(cleaned, cleaned+1, 10);
    cleaned[10] = '\0';
  }
  else if (len != 10)
    make_error(cleaned);
  else if (cleaned[0] < '2' || cleaned[3] < '2')
    make_error(cleaned);
  return cleaned;
}

static inline void *calloc_or_die(size_t nmemb, size_t size) {
  void *mem = calloc(nmemb, size);
  if (mem == NULL)
    exit(ENOMEM);
  return mem;
}

static void make_error(char *number) {
  static const char error[] = "0000000000";
  strcpy(number, error);
}

