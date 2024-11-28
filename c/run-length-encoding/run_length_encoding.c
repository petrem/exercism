#include "run_length_encoding.h"

#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *encode(const char *text) {
  if (text == NULL)
    return NULL;

  // on advice of counsel, instead of pre-computing the necessary space,
  // I'll re-allocate as needed
  size_t length = strlen(text);
  char *encoded = calloc(length + 1, 1);
  char *cursor_out = encoded;
  const char *cursor_in = text;
  char current;
  unsigned int repeat;
  size_t repeat_size;

  while ((current = *cursor_in)) {
    for(repeat = 0; *cursor_in == current; repeat++, cursor_in++);
    if (repeat > 1) {
      repeat_size = sprintf(cursor_out, "%u", repeat);
      if (repeat_size >= length)
        goto error;
      cursor_out += repeat_size;
      length -= repeat_size;
    }
    if (length == 0)
      goto error;
    *cursor_out++ = current;
    length--;
  }
    
  return realloc(encoded, cursor_out - encoded + 1);
 error:
  free(encoded);
  return NULL;
  
}

// This is set small so that possible bugs are more likely to crop up
#define DECODE_BUF_LEN 20

char *decode(const char *data) {
  if (data == NULL)
    return NULL;

  char *cursor_in = (char *)data;
  size_t length = DECODE_BUF_LEN;
  char *decoded = malloc(length + 1);
  size_t offset = 0;
  while (*cursor_in) {
    unsigned int repeat;
    int read_size;
    char current;
    if (!isspace(*cursor_in) && sscanf(cursor_in, "%u%n", &repeat, &read_size) == 1) {
      cursor_in += read_size;
    } else {
      repeat = 1;
    }
    current = *cursor_in++;
    if (length < repeat + 1) {
      // This is not efficient, when DECODE_BUF_LEN is small compared to the decoded text.
      // But I'll keep it simple.
      length = DECODE_BUF_LEN * ((repeat / DECODE_BUF_LEN) + 1);
      char *new_decoded = realloc(decoded, offset + length + 1);
      if (new_decoded == NULL) {
        free(decoded);
        return NULL;
      }
      decoded = new_decoded;
    }
    for (; repeat >= 1; repeat--) {
      decoded[offset++] = current;
      length -= 1;
    }
  }
  decoded[offset] = '\0';
  return realloc(decoded, offset + 1);
}
