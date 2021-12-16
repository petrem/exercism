#include "alloc.h"
#include <stdlib.h>
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

