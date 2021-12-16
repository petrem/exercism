#include "alloc.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>


void *malloc_or_die(size_t size) {
  void *ptr = malloc(size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}

void *calloc_or_die(size_t nmemb, size_t size) {
  void *ptr = calloc(nmemb, size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}

int realloc_safe(void **ptr, size_t old_size, size_t new_size) {
  if (ptr != NULL) {
    void *new_ptr = realloc(*ptr, new_size);
    if (new_ptr != NULL) {
      *ptr = new_ptr;
      if (new_size > old_size) {
        char *from = (char *)new_ptr + old_size;
        memset(from, 0, new_size - old_size);
      }
      return 0;
    }
  }
  return -1;
}
