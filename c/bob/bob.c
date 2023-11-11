#include "bob.h"
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#define R_SHOUT_QUESTION "Calm down, I know what I'm doing!"
#define R_QUESTION "Sure."
#define R_SHOUT "Whoa, chill out!"
#define R_SILENCE "Fine. Be that way!"
#define R_OTHER "Whatever."

static const char * strip_end(const char *phrase) {
  int len = strlen(phrase);
  if (len == 0)
    return NULL;
  const char *p;
  for (p = phrase + len - 1; p >= phrase; p--)
    if (!isspace(*p))
      break;
  if (p < phrase)
    return NULL;
  return p;
}

static bool is_question(const char *phrase) {
  const char *p = strip_end(phrase);
  return p != NULL && *p == '?';
}

static bool is_shout(const char * phrase) {
  const char *p;
  bool seen_upper = false;
  bool seen_lower = false;

  for (p = phrase; *p != '\0'; p++) {
    if (isalpha(*p) && islower(*p)) {
      seen_lower = true;
      break;
    }
    if (isupper(*p))
      seen_upper = true;
  }
  return seen_upper && !seen_lower;
}

static bool is_silence(const char * phrase) {
  return strip_end(phrase) == NULL;
}

const char *hey_bob(const char *phrase) {
  if (phrase == NULL) {
    exit(1);
  }

  const char *response;
  if (is_question(phrase))
    if (is_shout(phrase))
      response = R_SHOUT_QUESTION;
    else
      response = R_QUESTION;
  else
    if (is_shout(phrase))
        response = R_SHOUT;
    else
      if (is_silence(phrase))
        response = R_SILENCE;
      else
        response = R_OTHER;
  return response;
}
