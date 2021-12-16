#include "pig_latin.h"
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>

static inline bool isvowel(int c) {
  switch (c) {
  case 'y':
    return true;
  case 'a': case 'e': case 'i': case 'o': case 'u':
    return true;
  default:
    return false;
  }
}

static inline int not_isalnum(int c) {
  return !isalnum(c);
}

static inline const char *skip_on(const char *s, int (*fn)()) {
  while (*s && fn(*s)) s++;
  return s;
}

static size_t copy_word(char *dst, const char *src) {
  char *dp = dst;
  while (*src && isalnum(*src)) {
    *dp++ = *src++;
  }
  return dp - dst;
}

static size_t copy_nonword(char *dst, const char *src) {
  char *dp = dst;
  while (*src && not_isalnum(*src)) {
    *dp++ = *src++;
  }
  return dp - dst;
}

static size_t pigmay(char *dst, const char *word) {
  const char *src = word;

  switch (*src) {
    /* vowels */
  case 'a': case 'e': case 'i': case 'o': case 'u':
    break;
  case 'x':
    if (*(src+1) == 'r') break;
  case 'y':
    if (*(src+1) == 't') break;
  default:
    /* consonants */
    for(src++; !isvowel(*src); src++); //TODO: skip_on
    if (*src == 'u' && *(src-1) == 'q') {
      src++;
    }
  }
  char *dp = dst + copy_word(dst, src);
  while(word < src) {
    *dp++ = *word++;
  };
  dp += copy_word(dp, "ay");
  return dp - dst;
}


char *translate(const char *phrase) {
  if (phrase == NULL) {
    return NULL;
  }
  /* first count the length and words to allocate just enough memory */
  int n_words=0;
  const char *first_word = skip_on(phrase, not_isalnum);
  const char *p;
  for (p=first_word; *p; p=skip_on(skip_on(p, isalnum), not_isalnum)) {
    n_words++;
  }

  /* length of phrase + 'ay's for each word + final nul */
  char *response = calloc((p - phrase) + n_words * 2 + 1, 1);

  char *dp = response + copy_nonword(response, phrase);
  p = first_word;
  size_t copied;
  while (*p) {
    dp += pigmay(dp, p);
    p = skip_on(p, isalnum);
    copied = copy_nonword(dp, p);
    dp += copied;
    p += copied;
  }

  return response;
}
