// Pre-parse the inputs and compute required output size
// I wonder how this would compare with a realloc-based approach, or perhaps
//

#include "run_length_encoding.h"

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>


static unsigned int n_digits(unsigned int n);


char *encode(const char *text) {
  if (text == NULL)
    return NULL;

  const char *cursor_in = text;
  char current;
  unsigned int repeat;
  unsigned int length = 0;

  // pre-calculate required space

  while ((current = *cursor_in)) {
    for(repeat = 0; *cursor_in == current; repeat++, cursor_in++);
    length += 1 + n_digits(repeat);
  }
  
  char *encoded = calloc(length + 1, 1);
  char *cursor_out = encoded;
  cursor_in = text;
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

  char *cursor_in = (char *)data;
  unsigned int length = 0;
  while(*cursor_in) {
    unsigned int repeat;
    int read_size;
    if (!isspace(*cursor_in) && sscanf(cursor_in, "%u%n", &repeat, &read_size) == 1) {
      cursor_in += read_size;
    } else {
      repeat = 1;
    }
    cursor_in += 1;
    length += repeat;
  }
  char *decoded = calloc(length + 1, 1);
  cursor_in = (char *)data;
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

static unsigned int n_digits(unsigned int n) {
  return (n < 10 ? 1 :   
        (n < 100 ? 2 :   
        (n < 1000 ? 3 :   
        (n < 10000 ? 4 :   
        (n < 100000 ? 5 :   
        (n < 1000000 ? 6 :   
        (n < 10000000 ? 7 :  
        (n < 100000000 ? 8 :  
        (n < 1000000000 ? 9 :  
        10)))))))));
}
