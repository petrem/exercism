#include "beer_song.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>

/* Stern Warning: this solution is an Abomination Unto Nuggan.
 * It has come to be from craving the forbidden fruit of Bonus Pointus.
 * "Thou shalt remove as much duplication as thou shalt possibly can".
 */

#define REST(offset) (RESPONSE_SIZE - (offset))
#define PLURAL(n) n == 1 ? "" : "s"
#define BOTTLES "%1$s bottle%2$s of beer"
#define BOTTLES_OTW BOTTLES " on the wall"
#define DRINK "Take %3$s down and pass it around, "


static size_t append(char *ptr, int offset, const char *fmt, ...) {
  va_list args;
  int written;

  if (REST(offset) <= 0 || ptr == NULL || fmt == NULL) {
    exit(1);
  }
  va_start(args, fmt);
  written = vsnprintf(ptr + offset, REST(offset), fmt, args);
  va_end(args);
  if (written < 0 || written > REST(offset)) {
    exit(1);
  }
  return written;
}


static char *beers_str(int n) {
  static char beers_str[3];
  if (n == 0) {
    return "no more";
  }
  snprintf(beers_str, sizeof(beers_str), "%d", n);
  return beers_str;
}


int verse(char *response, int beers) {
  if (response == NULL) {
    exit(1);
  }
  int written = 0;
  char *take_what = "one";
  int next_beers = beers - 1;
  char * next_action = DRINK BOTTLES_OTW ".\n";

  written += append(response, written,
                    BOTTLES_OTW ", " BOTTLES ".\n",
                    beers_str(beers), PLURAL(beers));
  response[0] = toupper(response[0]);
  switch (beers) {
  case 1:
    take_what = "it"; break;
  case 0:
    next_beers = 99;
    next_action = "Go to the store and buy some more, " BOTTLES_OTW ".\n";
    break;
  }
  written += append(response, written,
                    next_action,
                    beers_str(next_beers), PLURAL(next_beers), take_what);
  return written;
}

void sing(char *response, int from, int to) {
  if (response == NULL) {
    exit(1);
  }
  int written = 0;
  for (; from >= to; from--) {
    written += verse(response + written, from);
    written += append(response, written, "\n");
  }
  *(response+written-1) = '\0';
}

