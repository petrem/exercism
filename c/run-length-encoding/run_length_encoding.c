// with RUBBISH memory handling

#include "run_length_encoding.h"

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

char *encode(const char *text) {
  if (text == NULL)
    return NULL;
  char *encoded = calloc(100, 1);
  const char *cursor_in = text;
  char *cursor_out = encoded, current;
  unsigned int repeat = 0;

  while ((current=*cursor_in)) {
    for(repeat = 0; *cursor_in == current; repeat++, cursor_in++);
    if (repeat > 1) {
      cursor_out += sprintf(cursor_out, "%u", repeat);
    }
    *cursor_out++ = current;
  }
  return encoded;
}


char *decode(const char *data) {
  if (data == NULL)
    return NULL;
  char *decoded = calloc(100, 1);
  const char *cursor_in = data;
  char *cursor_out = decoded;
  while(*cursor_in) {
    unsigned int repeat;
    int read_size;
    char current;
    if (!isspace(*cursor_in) && sscanf(cursor_in, "%u%n", &repeat, &read_size) == 1) {
      cursor_in += read_size;
    } else {
      repeat = 1;
    }
    current = *cursor_in++;
    for (; repeat >= 1; repeat--) {
      *cursor_out++ = current;
    }
  }
  return decoded;
}
