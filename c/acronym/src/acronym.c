#include "acronym.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <stdio.h>


char *abbreviate(const char *phrase) {
  if (phrase == NULL)
    return NULL;

  char * acronym = malloc(strlen(phrase) + 1);
  char *ap = acronym;
  char prev = ' ';

  for(char *c = (char *)phrase; *c; prev = *c, c++) {
    if (isalpha(*c) && prev != '\'' && (isspace(prev) || ispunct(prev)))
     *ap++ = (unsigned char)toupper(*c);
  }
  if (ap == acronym) {
    free(acronym);
    return NULL;
  }
  *ap = '\0';
  return acronym;
}
