#include "atbash_cipher.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include <stdio.h>

static void * calloc_or_die(size_t nmemb, size_t size);

static char atbash_transform(char c);

char *atbash_encode(char *message) {
  int message_len = strlen(message);
  char *encoded = calloc_or_die(message_len + message_len / 5 + 1, 1);
  char *p = encoded;
  int counter = 6;
  char c;

  for (; *message ; message++) {
    c = atbash_transform(*message);
    if (c != '\0') {
      counter--;
      if (counter == 0) {
        *p++ = ' ';
        counter = 5;
      }
      *p++ = c;
    }
  }
  return encoded;
}

char *atbash_decode(char *message) {
  int message_len = strlen(message);
  char *decoded = calloc_or_die(message_len - message_len / 5 + 3, 1);
  char *p = decoded;
  char c;
  for (; *message; message++) {
    c = atbash_transform(*message);
    if (c != '\0')
      *p++ = c;
  }
  return decoded;
}


static void * calloc_or_die(size_t nmemb, size_t size) {
  void *mem = calloc(nmemb, size);
  if (mem == NULL) {
    exit(ENOMEM);
  }
  return mem;
}


static char atbash_transform(char c) {
  if (isalpha(c))
    return 'z' - tolower(c) + 'a';
  if (isdigit(c))
    return c;
  return '\0';
}
